/*

Copyright 2008-2014 E-Hentai.org
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

import java.io.File
import java.net.InetAddress

//import scala.collection.mutable.HashMap

object Settings {
  def NEWLINE = System.getProperty("line.separator")

  // the client build is among others used by the server to determine the client's capabilities.
  // any forks should use the build number as an indication of compatibility with mainline, and not use it as an internal build number.
  def CLIENT_BUILD = 96
  def CLIENT_VERSION = "1.2.5"

  var CLIENT_API_URL = "http://rpc.hentaiathome.net/clientapi.php?"

  def DATA_FILENAME_CLIENT_LOGIN = "client_login"
  def DATA_FILENAME_LASTHIT_HISTORY = "lasthit_history"

  def CLIENT_KEY_LENGTH = 20
  def MAX_KEY_TIME_DRIFT = 300
  def MAX_CONNECTION_BASE = 20

  var CONTENT_TYPE_DEFAULT = "text/html charset=iso-8859-1"
  var CONTENT_TYPE_OCTET = "application/octet-stream"
  var CONTENT_TYPE_JPG = "image/jpeg"
  var CONTENT_TYPE_PNG = "image/png"
  var CONTENT_TYPE_GIF = "image/gif"

  var TCP_PACKET_SIZE_HIGH = 1460
  var TCP_PACKET_SIZE_LOW = 536
  var MAX_REQUEST_LENGTH = 10000

  private var activeClient: HentaiAtHomeClient = null
  private var activeGUI: HathGUI = null

  private var clientID = 0
  private var clientKey = ""
  private var serverTimeDelta = 0

  private var rpcChangeMonitor = new Object()
  private var rpcServers: Array[InetAddress] = null
  private var imageServer = ""
  private var clientName = ""
  private var clientHost = ""
  private var clientPort = 0
  private var requestServer = ""
  private var requestProxyMode = 0

  private var throttle_bytes = 0
  private var hourbwlimit_bytes:Long = 0
  private var disklimit_bytes:Long = 0
  private var diskremaining_bytes:Long = 0

  // read from command-line arguments

  private var forceDirty = false
  private var verifyCache = false
  private var skipFreeSpaceCheck = false
  private var warnNewClient = false
  private var useLessMemory = false
  private var disableBWM = false

  private var overrideConns = 0

  private var datadir:File = null

  private var staticRanges:Set[String] = Set() //Map[String, Int] = Map()

  def setActiveClient(client: HentaiAtHomeClient) { activeClient = client }
  def setActiveGUI(gui: HathGUI) { activeGUI = gui }

  def loginCredentialsAreSyntaxValid():Boolean = {
      clientID > 0 && java.util.regex.Pattern.matches("^[a-zA-Z0-9]{" + Settings.CLIENT_KEY_LENGTH + "}$", clientKey)
  }

  def loadClientLoginFromFile():Boolean = {
    val clientLogin = new File(Settings.getDataDir(), Settings.DATA_FILENAME_CLIENT_LOGIN)
    if(!clientLogin.exists()) {
      return false
    }
    try {
      val filecontent = FileTools.getStringFileContents(clientLogin)
      if(!filecontent.isEmpty()) {
        val split = filecontent.split("-", 2)
        if(split.length == 2) {
          clientID = split(0).toInt
          clientKey = split(1)
          return true
        }
      }
    } catch{
      case e:Exception =>
        Out.warning("Encountered error when reading " + Settings.DATA_FILENAME_CLIENT_LOGIN + ": " + e)
    }
    return false
  }

  def promptForIDAndKey(iqh: InputQueryHandler) {
    Out.info("Before you can use this client, you will have to register it at http://hentaiathome.net/")
    Out.info("IMPORTANT: YOU NEED A SEPARATE IDENT FOR EACH CLIENT YOU WANT TO RUN.")
    Out.info("DO NOT ENTER AN IDENT THAT WAS ASSIGNED FOR A DIFFERENT CLIENT.")
    Out.info("After registering, enter your ID and Key below to start your client.")
    Out.info("(You will only have to do this once.)\n")

    clientID = 0
    clientKey = ""

    do {
      try {
        clientID = iqh.queryString("Enter Client ID").trim().toInt
      } catch {
        case nfe: java.lang.NumberFormatException => Out.warning("Invalid Client ID. Please try again.")
      }
    } while(clientID < 1000)

    do {
      clientKey = iqh.queryString("Enter Client Key").trim()
      if(!loginCredentialsAreSyntaxValid()) {
        Out.warning("Invalid Client Key, it must be exactly 20 alphanumerical characters. Please try again.")
      }
    } while(!loginCredentialsAreSyntaxValid())

    try {
      FileTools.putStringFileContents(new File(Settings.getDataDir(), Settings.DATA_FILENAME_CLIENT_LOGIN), clientID + "-" + clientKey)
    } catch{
      case ioe: java.io.IOException =>
        Out.warning("Error encountered when writing " + Settings.DATA_FILENAME_CLIENT_LOGIN + ": " + ioe)
    }
  }

  def parseAndUpdateSettings(settings: Array[String]):Boolean = {
    if(settings == null) {
      return false
    }
    for(s <- settings) {
      if(s != null) {
        val split = s.split("=", 2)
        if(split.length == 2) {
          updateSetting(split(0).toLowerCase(), split(1))
        }
      }
    }
    return true
  }

  // note that these settings will currently be overwritten by any equal ones read from the server, so it should not be used to override server-side settings.
  def parseArgs(args: Array[String]):Boolean = {
    if(args == null) {
      return false
    }

    for(s <- args) {
      if(s != null) {
        if(s.startsWith("--")) {
          val split = s.substring(2).split("=", 2)
          val two = if(split.length == 2) split(1) else "true"
          updateSetting(split(0).toLowerCase(), two)
        } else {
          Out.warning("Invalid command argument: " + s)
        }
      }
    }

    return true
  }

  def updateSetting(_setting:String, value:String):Boolean = {
    val setting = _setting.replace("-", "_")

    try {
      setting match {
        case "min_client_build" =>
          if(value.toInt > CLIENT_BUILD) {
            HentaiAtHomeClient.dieWithError("Your client is too old to connect to the Hentai@Home Network. Please download the new version of the client from http://hentaiathome.net/")
          }
        case "cur_client_build" =>
          if(value.toInt > CLIENT_BUILD) {
            warnNewClient = true
          }
        case "server_time" => {
          serverTimeDelta = value.toInt - (System.currentTimeMillis() / 1000).toInt
          Out.debug("Setting altered: serverTimeDelta=" + serverTimeDelta)
          return true
        }
        case "rpc_server_ip" =>
          rpcChangeMonitor.synchronized {
            rpcServers = value.split(";").map(java.net.InetAddress.getByName(_))
          }
        case "image_server" => imageServer = value
        case "name" => clientName = value
        case "host" => clientHost = value
        case "port" => clientPort = value.toInt
        case "request_server" => requestServer = value
        case "request_proxy_mode" => requestProxyMode = value.toInt

        // THIS SHOULD NOT BE ALTERED BY THE CLIENT AFTER STARTUP.
        // Using the website interface will update the throttle value for the dispatcher first, and update the client on the first stillAlive test.
        case "throttle_bytes" => throttle_bytes = value.toInt
        case "hourbwlimit_bytes" =>
            // see above
            hourbwlimit_bytes = value.toLong
        case "disklimit_bytes" => {
          val newLimit = value.toLong
          if(newLimit >= disklimit_bytes) {
            disklimit_bytes = newLimit
          } else {
            Out.warning("The disk limit has been reduced. However, this change will not take effect until you restart your client.")
          }
        }
        case "diskremaining_bytes" => diskremaining_bytes = value.toLong
        case "force_dirty" => forceDirty = value == "true"
        case "verify_cache" => verifyCache = value == "true"
        case "use_less_memory" => useLessMemory = value == "true"
        case "disable_logging" => Out.disableLogging()
        case "disable_bwm" => disableBWM = value == "true"
        case "skip_free_space_check" => skipFreeSpaceCheck = value == "true"
        case "max_connections" => overrideConns = value.toInt
        case "static_ranges" => {
          staticRanges = value.split(";").filter(_.length() == 4).toSet //.map({(s:String) => (s, 1)})
        }
        case "silentstart" => // pass
        case _ =>
          // don't flag errors if the setting is handled by the GUI
          Out.warning("Unknown setting " + setting + " = " + value)
          return false
      }

      Out.debug("Setting altered: " + setting +"=" + value)
      return true
    } catch{
      case e: Exception => Out.warning("Failed parsing setting " + setting + " = " + value)
    }
    return false
  }

  def initializeDataDir() /*throws java.io.IOException*/ {
    datadir = FileTools.checkAndCreateDir(new File("data"))
  }

  // accessor methods
  def getDataDir() = datadir
  def getClientID() = clientID
  def getClientKey() =  clientKey
  def getImageServer(fileid: String) = imageServer
  def getClientName() = clientName
  def getClientHost() = clientHost
  def getClientPort() = clientPort
  def getRequestServer() = requestServer
  def getRequestProxyMode() = requestProxyMode
  def getThrottleBytesPerSec() = throttle_bytes
  def getHourBWLimitBytes() = hourbwlimit_bytes
  def getDiskLimitBytes() = disklimit_bytes
  def getDiskMinRemainingBytes() = diskremaining_bytes
  def getServerTime():Int = (System.currentTimeMillis() / 1000).toInt + serverTimeDelta
  def getOutputLogPath() = "data/log_out"
  def getErrorLogPath() = "data/log_err"

  def isForceDirty() = forceDirty
  def isVerifyCache() = verifyCache
  def isUseLessMemory() =  useLessMemory
  def isSkipFreeSpaceCheck() = skipFreeSpaceCheck
  def isWarnNewClient() = warnNewClient
  def isDisableBWM() = disableBWM

  def getActiveClient() =  activeClient
  def getActiveGUI() =  activeGUI

  def isValidRPCServer(compareTo: InetAddress):Boolean = {
    rpcChangeMonitor.synchronized {
      if(rpcServers == null) {
        return false
      }
      for(i <- rpcServers) {
        if(i.equals(compareTo)) {
          return true
        }
      }
      return false
    }
  }

  def getMaxConnections():Int = {
    if(overrideConns > 0) {
      overrideConns
    } else {
      val uptime = Stats.getUptime()
      val conns = if(throttle_bytes > 0) {
        MAX_CONNECTION_BASE + (throttle_bytes / 10000).toInt
      } else if(uptime > 0) {
        // to be safe, we'll assume that each connection takes 120 seconds to finish. so 1 connection per second = 120 connections.
        (Stats.getFilesSent() * 120 / uptime).toInt
      } else 0

      Math.max(Math.min(500, conns), MAX_CONNECTION_BASE)
    }
  }

  def isStaticRange(fileid: String) = staticRanges.contains(fileid.substring(0, 4))
    //if(staticRanges == null) false
    //else staticRanges.contains(fileid.substring(0, 4))

  def getStaticRangeCount() = staticRanges.size
    //if(staticRanges == null) 0
    //else staticRanges.size
}
