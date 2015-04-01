/*

Copyright 2008-2012 E-Hentai.org
http://forums.e-hentai.org/
ehentai@gmail.com

This file is part of Hentai@Home.

Hentai@Home is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Hentai@Home is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Hentai@Home.  If not, see <http://www.gnu.org/licenses/>.

*/

package org.hath.base

import java.net.Socket
import java.net.ServerSocket
import java.net.InetAddress
import java.lang.Thread

class HTTPServer(client:HentaiAtHomeClient) extends Runnable {
  //private HentaiAtHomeClient client
  private val bandwidthMonitor = new HTTPBandwidthMonitor()
  private var ss:ServerSocket = null
  private var myThread:Thread = null
  private var sessions: List[HTTPSession] = List()
  private var currentConnId = 0
  private var isAllowNormalConnections = false
  private var floodControlTable:Map[String, FloodControlEntry] = Map()

  def startConnectionListener(port:Int):Boolean = {
    try {
      Out.info("Starting up the internal HTTP Server...")
      if(ss != null) {
          stopConnectionListener()
      }
      ss = new ServerSocket(port)
      myThread = new Thread(this)
      myThread.start()
      Out.info("Internal HTTP Server was successfully started, and is listening on port " + port)
      return true
    } catch{ case e:Exception => {
        allowNormalConnections()
        e.printStackTrace()
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("Could not start the internal HTTP server.")
        Out.info("This is most likely caused by something else running on the port H@H is trying to use.")
        Out.info("In order to fix this, either shut down whatever else is using the port, or assign a different port to H@H.")
        Out.info("************************************************************************************************************************************")
        Out.info("")
      }
    }
    return false
  }

  def stopConnectionListener() {
    if(ss != null) {
      try {
        ss.close() // will cause ss.accept() to throw an exception, terminating the accept thread
      } catch {case e: Exception => {}}
      ss = null
    }
  }

  def pruneFloodControlTable() {
    floodControlTable.synchronized {
      floodControlTable = floodControlTable.filter({case (k, v) => !v.isStale })
    }
  }

  def nukeOldConnections(killall:Boolean) {
    sessions.synchronized {
      // in some rare cases, the connection is unable to remove itself from the session list.
      // if so, it will return true for doTimeoutCheck, meaning that we will have to clear it out from here instead
      sessions = sessions.filter(!_.doTimeoutCheck(killall))
    }
  }

  def allowNormalConnections() {
    isAllowNormalConnections = true
  }

  def run() {
    try {
      while(true) {
        val s = ss.accept()

        sessions.synchronized {
          var forceClose = false

          //  private network: localhost, 127.x.y.z, 10.0.0.0 - 10.255.255.255, 172.16.0.0 - 172.31.255.255,  192.168.0.0 - 192.168.255.255, 169.254.0.0 -169.254.255.255

          val addr = s.getInetAddress()
          val addrString = addr.toString()
          val myInetAddr = Settings.getClientHost().replace("::ffff:", "")
          val localNetworkAccess = java.util.regex.Pattern.matches("^((" + myInetAddr + ")|(localhost)|(127\\.)|(10\\.)|(192\\.168\\.)|(172\\.((1[6-9])|(2[0-9])|(3[0-1]))\\.)|(169\\.254\\.)|(::1)|(0:0:0:0:0:0:0:1)|(fc)|(fd)).*$", addr.getHostAddress())
          val apiServerAccess = Settings.isValidRPCServer(addr)

          if(!apiServerAccess && !isAllowNormalConnections) {
            Out.warning("Rejecting connection request during startup.")
            forceClose = true
          } else if(!apiServerAccess && !localNetworkAccess) {
            // connections from the API Server and the local network are not subject to the max connection limit or the flood control

            val maxConnections = Settings.getMaxConnections()
            val currentConnections = sessions.size

            if(currentConnections > maxConnections) {
                Out.warning("Exceeded the maximum allowed number of incoming connections (" + maxConnections + ").")
                forceClose = true
            } else {
              if(currentConnections > maxConnections * 0.8) {
                // let the dispatcher know that we're close to the breaking point.
                // this will make it back off for 30 sec, and temporarily turns down the dispatch rate to half.
                client.getServerHandler().notifyOverload()
              }

              // this flood control will stop clients from opening more than ten connections over a (roughly) five second floating window,
              // and forcibly block them for 60 seconds if they do.
              var fce = floodControlTable.synchronized {
                floodControlTable.get(addrString) match {
                  case Some(el) => el
                  case _ => {
                    val el = new FloodControlEntry(addr)
                    floodControlTable += addrString -> el
                    el
                  }
                }
              }

              if(!fce.isBlocked()) {
                if(!fce.hit()) {
                  Out.warning("Flood control activated for  " + addrString + " (blocking for 60 seconds)")
                  forceClose = true
                }
              } else {
                forceClose = true
              }
            }
          }

          if(forceClose) {
            try { s.close() } catch{ case e:Exception => { /* LALALALALA */ }}
          }else {
            try {
              s.setReceiveBufferSize(131072)
              s.setSendBufferSize(131072)
              s.setTrafficClass(8)
            } catch { case e:java.net.SocketException =>
              Out.debug("Could not set socket buffers: " + e.getMessage())
            }

            // all is well. keep truckin'
            val hs = new HTTPSession(s, getNewConnId(), localNetworkAccess, this)
            sessions :+= hs
            Stats.setOpenConnections(sessions.size)
            hs.handleSession()
          }
        }
      }
    } catch { case e: java.io.IOException => {
        if(!client.isShuttingDown()) {
          Out.error("ServerSocket terminated unexpectedly!")
          HentaiAtHomeClient.dieWithError(e)
        } else {
          Out.info("ServerSocket was closed and will no longer accept new connections.")
        }
        ss = null
      }
    }
  }

  private def getNewConnId() = {
    this.synchronized {
      currentConnId += 1
      currentConnId
    }
  }

  def removeHTTPSession(httpSession:HTTPSession) {
    sessions.synchronized {
      //sessions.remove(httpSession)
      sessions = sessions diff List(httpSession)
      Stats.setOpenConnections(sessions.size)
    }
  }

  def getBandwidthMonitor() = bandwidthMonitor
  def getHentaiAtHomeClient() = client

  private class FloodControlEntry(addr:InetAddress) {
    private var connectCount = 0
    private var lastConnect:Long = 0
    private var blocktime:Long = 0

    def isStale() = lastConnect < System.currentTimeMillis() - 60000
    def isBlocked() = blocktime > System.currentTimeMillis()
    def hit():Boolean = {
      val nowtime = System.currentTimeMillis()
      connectCount = Math.max(0, connectCount - Math.floor((nowtime - lastConnect) / 1000).toInt).toInt + 1
      lastConnect = nowtime

      if(connectCount <= 10) true else {
        // block this client from connecting for 60 seconds
        blocktime = nowtime + 60000
        false
      }
    }
  }
}
