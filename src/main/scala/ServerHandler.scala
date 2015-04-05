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

import java.net.URL
import java.io.File
import java.lang.StringBuffer
import java.util.ArrayList
import java.util.Collections
import java.util.Enumeration
import java.util.Hashtable
//import java.util.List
import java.io.BufferedReader
import java.io.InputStreamReader

import scala.collection.mutable.MutableList

object ServerHandler {
  private var loginValidated = false

  def isLoginValidated() = loginValidated

  val ACT_GET_BLACKLIST = "get_blacklist"
  val ACT_SERVER_STAT = "server_stat"

  val ACT_CLIENT_LOGIN = "client_login"
  val ACT_CLIENT_SETTINGS = "client_settings"

  val ACT_CLIENT_START = "client_start"
  val ACT_CLIENT_SUSPEND = "client_suspend"
  val ACT_CLIENT_RESUME = "client_resume"
  val ACT_CLIENT_STOP = "client_stop"
  val ACT_STILL_ALIVE = "still_alive"
  val ACT_FILE_UNCACHE = "file_uncache"
  val ACT_FILE_REGISTER = "file_register"
  val ACT_MORE_FILES = "more_files"
  val ACT_FILE_TOKENS = "download_list"
  val ACT_OVERLOAD = "overload"

  def getServerConnectionURL(act:String):URL = getServerConnectionURL(act, "")
  def getServerConnectionURL(act:String, add:String):URL = {
    var serverConnectionURL:URL = null

    try {
      if(act.equals(ACT_SERVER_STAT)) {
        serverConnectionURL = new java.net.URL(Settings.CLIENT_API_URL + "clientbuild=" + Settings.CLIENT_BUILD + "&act=" + act)
      } else {
        val correctedTime = Settings.getServerTime()
        val actkey = MiscTools.getSHAString("hentai@home-" + act + "-" + add + "-" + Settings.getClientID() + "-" + correctedTime + "-" + Settings.getClientKey())
        serverConnectionURL = new java.net.URL(Settings.CLIENT_API_URL + "clientbuild=" + Settings.CLIENT_BUILD + "&act=" + act + "&add=" + add + "&cid=" + Settings.getClientID() + "&acttime=" + correctedTime + "&actkey=" + actkey)
      }
    } catch {case e:java.net.MalformedURLException =>
      HentaiAtHomeClient.dieWithError(e)
    }

    serverConnectionURL
  }
}

class ServerHandler(client:HentaiAtHomeClient) {

  // communications that do not use additional variables can use this
  private def simpleNotification(act:String, humanReadable:String):Boolean = {
    val sr = ServerResponse.getServerResponse(act, this)
    if(sr.isOk()) {
      Out.debug(humanReadable + " notification successful.")
      true
    } else {
      Out.warning(humanReadable + " notification failed.")
      false
    }
  }

  // simple notifications

  def notifySuspend() = simpleNotification(ServerHandler.ACT_CLIENT_SUSPEND, "Suspend")
  def notifyResume() = simpleNotification(ServerHandler.ACT_CLIENT_RESUME, "Resume")
  def notifyShutdown() = simpleNotification(ServerHandler.ACT_CLIENT_STOP, "Shutdown")

  private var lastOverloadNotification:Long = 0
  def notifyOverload() = {
    val nowtime = System.currentTimeMillis()

    if(lastOverloadNotification < nowtime - 30000) {
      lastOverloadNotification = nowtime
      simpleNotification(ServerHandler.ACT_OVERLOAD, "Overload")
    } else false
  }

  def notifyMoreFiles() = simpleNotification(ServerHandler.ACT_MORE_FILES, "More Files")

  // these communcation methods are more complex, and have their own result parsing

  def notifyStart():Boolean = {
    val sr = ServerResponse.getServerResponse(ServerHandler.ACT_CLIENT_START, this)

    if(sr.isOk()) {
        Out.info("Start notification successful. Note that there may be a short wait before the server registers this client on the network.")
        true
    } else {
      val failcode = sr.getFailCode()
      if(failcode.startsWith("FAIL_CONNECT_TEST")) {
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("The client has failed the external connection test.")
        Out.info("The server failed to verify that this client is online and available from the Internet.")
        Out.info("If you are behind a firewall, please check that port " + Settings.getClientPort() + " is forwarded to this computer.")
        Out.info("You might also want to check that " + Settings.getClientHost() + " is your actual IP address.")
        Out.info("If you need assistance with forwarding a port to this client, locate a guide for your particular router at http://portforward.com/")
        Out.info("The client will remain running so you can run port connection tests.")
        Out.info("Use Program -> Exit in windowed mode or hit Ctrl+C in console mode to exit the program.")
        Out.info("************************************************************************************************************************************")
        Out.info("")

        return false
      }
      else if(failcode.startsWith("FAIL_STARTUP_FLOOD")) {
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("Flood control is in effect.")
        Out.info("The client will automatically retry connecting in 90 seconds.")
        Out.info("************************************************************************************************************************************")
        Out.info("")

        try {
            java.lang.Thread.sleep(90000)
        } catch{case e:Exception => }

        return notifyStart()
      }
      else if(failcode.startsWith("FAIL_OTHER_CLIENT_CONNECTED")) {
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("The server detected that another client was already connected from this computer or local network.")
        Out.info("You can only have one client running per IP address.")
        Out.info("The program will now terminate.")
        Out.info("************************************************************************************************************************************")
        Out.info("")

        HentaiAtHomeClient.dieWithError("FAIL_OTHER_CLIENT_CONNECTED")
      }
      else if(failcode.startsWith("FAIL_CID_IN_USE")) {
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("The server detected that another client is already using this client ident.")
        Out.info("If you want to run more than one client, you have to apply for additional idents.")
        Out.info("The program will now terminate.")
        Out.info("************************************************************************************************************************************")
        Out.info("")

        HentaiAtHomeClient.dieWithError("FAIL_CID_IN_USE")
      }
      else if(failcode.startsWith("FAIL_RESET_SUSPENDED")) {
        Out.info("")
        Out.info("************************************************************************************************************************************")
        Out.info("This client ident has been revoked for having too many cache resets.")
        Out.info("The program will now terminate.")
        Out.info("************************************************************************************************************************************")
        Out.info("")

        HentaiAtHomeClient.dieWithError("FAIL_RESET_SUSPENDED")
      }
      false
    }
  }

  // TODO
  def notifyUncachedFiles(deletedFiles:List[HVFile]) {
    // note: as we want to avoid POST, we do this as a long GET. to avoid exceeding certain URL length limitations, we uncache at most 50 files at a time
    var deleteCount = deletedFiles.size

    if(deleteCount > 0) {
      Out.debug("Notifying server of " + deleteCount + " uncached files...")

      do {
        var sb = new StringBuffer()
        var limiter = 1
        while(deleteCount > 0 && limiter <= 50) {
          val delim = if(limiter != 1)  ";" else ""
          deleteCount -= 1
          sb.append(delim + deletedFiles/*.remove*/(deleteCount).getFileid())
          limiter+=1
        }

        val uncacheURL = ServerHandler.getServerConnectionURL(ServerHandler.ACT_FILE_UNCACHE, sb.toString())
        val sr = ServerResponse.getServerResponse(uncacheURL, this)

        if(sr.isOk()) {
          Out.debug("Uncache notification successful.")
        } else {
          Out.warning("Uncache notification failed.")
        }
      } while(deleteCount > 0)
    }
  }

  def notifyRegisterFiles(pendingRegister:List[HVFile]) {
    var registerCount = pendingRegister.size

    Out.debug("Notifying server of " + registerCount + " registered files...")

    var sb = new StringBuffer()
    while(registerCount > 0) {
      registerCount -= 1
      val delim = if(sb.length() > 0) ";" else ""
      sb.append(delim + pendingRegister/*.remove*/(registerCount).getFileid())
    }

    val registerURL = ServerHandler.getServerConnectionURL(ServerHandler.ACT_FILE_REGISTER, sb.toString())
    val sr = ServerResponse.getServerResponse(registerURL, this)

    if(sr.isOk()) {
      Out.debug("Register notification successful.")
    } else {
      Out.warning("Register notification failed.")
    }
  }

  def getBlacklist(deltatime:Long): Array[String] = {
    val blacklistURL = ServerHandler.getServerConnectionURL(ServerHandler.ACT_GET_BLACKLIST, "" + deltatime)
    val sr = ServerResponse.getServerResponse(blacklistURL, this)

    if(sr.isOk()) sr.getResponseText() else null
  }

  def stillAliveTest() {
    val cs = new CakeSphere(this, client)
    cs.stillAlive()
  }

  // this MUST NOT be called after the client has started up, as it will clear out and reset the client on the server, leaving the client in a limbo until restart
  def loadClientSettingsFromServer() {
    Stats.setProgramStatus("Loading settings from server...")
    Out.info("Connecting to the Hentai@Home Server to register client with ID " + Settings.getClientID() + "...")

    try {
      do {
        if(!refreshServerStat()) {
          HentaiAtHomeClient.dieWithError("Failed to get initial stat from server.")
        }

        Out.info("Reading Hentai@Home client settings from server...")
        val sr = ServerResponse.getServerResponse(ServerHandler.ACT_CLIENT_LOGIN, this)

        if(sr.isOk()) {
          ServerHandler.loginValidated = true
          Out.info("Applying settings...")
          Settings.parseAndUpdateSettings(sr.getResponseText())
          Out.info("Finished applying settings")
        } else if(sr.isNull()) {
          HentaiAtHomeClient.dieWithError("Failed to get a login response from server.")
        } else {
            Out.warning("\nAuthentication failed, please re-enter your Client ID and Key (Code: " + sr.getFailCode() + ")")
            Settings.promptForIDAndKey(client.getInputQueryHandler())
        }
      } while(!ServerHandler.loginValidated)
    } catch{case e:Exception =>
      HentaiAtHomeClient.dieWithError(e)
    }
  }

  def refreshServerSettings() = {
    Out.info("Refreshing Hentai@Home client settings from server...")
    val sr = ServerResponse.getServerResponse(ServerHandler.ACT_CLIENT_SETTINGS, this)

    if(sr.isOk()) {
      Settings.parseAndUpdateSettings(sr.getResponseText())
      Out.info("Finished applying settings")
      //client.getCacheHandler().recheckFreeDiskSpace()  - we're not bothering to recheck the free space as the client doesn't accept live reductions of disk space
      true
    } else {
      Out.warning("Failed to refresh settings")
      false
    }
  }

  def refreshServerStat() = {
    Stats.setProgramStatus("Getting initial stats from server...")
    // get timestamp and minimum client build from server
    val sr = ServerResponse.getServerResponse(ServerHandler.ACT_SERVER_STAT, this)
    if(sr.isOk()) {
      Settings.parseAndUpdateSettings(sr.getResponseText())
      true
    } else {
      false
    }
  }

  def getFileTokens(requestTokens:List[String]):Hashtable[String,String] = {
      var tokens = ""

      for(token <- requestTokens) {
        tokens = tokens.concat(token + "")
      }

      val tokenURL = ServerHandler.getServerConnectionURL(ServerHandler.ACT_FILE_TOKENS, tokens)
      val sr = ServerResponse.getServerResponse(tokenURL, this)

      if(sr.isOk()) {
        var tokenTable = new Hashtable[String,String]()
        val split = sr.getResponseText()

        for(s <- split) {
          if(! s.isEmpty()) {
            val s2 = s.split(" ", 2)
            tokenTable.put(s2(0), s2(1))
          }
        }

        tokenTable
      } else {
        Out.info("Could not grab token list - most likely the client has not been qualified yet. Will retry in a few minutes.")
        null
      }
  }



  // these functions do not communicate with the RPC server, but are actions triggered by it through servercmd

  def downloadFilesFromServer(files:Hashtable[String,String]):String = {
    val returnText = new StringBuffer()
    val fileids = files.keys()

    try {
      while(fileids.hasMoreElements()) {
        val file = fileids.nextElement()
        val key = files.get(file)

        val s = file.split(":")
        val fileid = s(0)
        val host = s(1)

        // verify that we have valid ID and Key before we build an URL from it, in case the server has been compromised somehow...
        if(HVFile.isValidHVFileid(fileid) && key.matches("^[0-9]{6}-[a-z0-9]{40}$")) {
          val source = new URL("http", host, 80, "/image.php?f=" + fileid + "&t=" + key)

          if(downloadAndCacheFile(source, fileid)) {
            returnText.append(fileid + ":OK\n")
          } else {
            returnText.append(fileid + ":FAIL\n")
          }
        } else {
          returnText.append(fileid + ":INVALID\n")
        }
      }
    } catch{case e:Exception => {
      e.printStackTrace()
      Out.warning("Encountered error " + e + " when downloading image files from server. Will not retry.")
    }}

    returnText.toString()
  }

  def doThreadedProxyTest(ipaddr:String, port:Int, testsize:Int, testcount:Int, testtime:Int, testkey:String):String = {
    var successfulTests = 0
    var totalTimeMillis:Long = 0

    Out.debug("Running threaded proxy test against ipaddr=" + ipaddr + " port=" + port + " testsize=" + testsize + " testcount=" + testcount + " testtime=" + testtime + " testkey=" + testkey)

    try {
      (0 to testcount-1).map({(i:Int) =>
        val q = "/t/" + testsize + "/" + testtime + "/" + testkey + "/" + Math.floor(Math.random().toInt * Integer.MAX_VALUE)
        val source = new URL("http", ipaddr, port, q)
        Out.debug("Test thread: " + source)
        val dler = new FileDownloader(source, 10000, 60000)
        dler.startAsyncDownload()
        dler
      }).filter(_.waitAsyncDownload()).foreach({(dler:FileDownloader) =>
        successfulTests += 1
        totalTimeMillis += dler.getDownloadTimeMillis()
      })
    } catch{case e:java.net.MalformedURLException =>
      HentaiAtHomeClient.dieWithError(e)
    }

    "OK:" + successfulTests + "-" + totalTimeMillis
  }

  // make an educated guess on OS to access the built-in ping utility
  private def pingcmd(ipaddr:String):String = {
    val whichOS = System.getProperty("os.name")
    if(whichOS != null && whichOS.toLowerCase().indexOf("windows") > -1) {
      // windows style
      "ping -n 3 " + ipaddr
    } else {
      // linux/unix/bsd/macos style
      "ping -c 3 " + ipaddr
    }
  }

  def doProxyTest(ipaddr:String, port:Int, fileid:String, keystamp:String):String = {
    if(!HVFile.isValidHVFileid(fileid)) {
      Out.error("Encountered an invalid fileid in doProxyTest: " + fileid)
      return fileid + ":INVALID-0"
    }

    try {
      val source = new URL("http", ipaddr, port, "/h/" + fileid + "/keystamp=" + keystamp + "/test.jpg")
      Out.info("Running a proxy test against " + source + ".")

      // determine the approximate ping time to the other client (if available, done on a best-effort basis). why isn't there a built-in ping in java anyway?
      var pingtime = 0

      // juuuuuust in case someone manages to inject a faulty IP address, we don't want to pass that unsanitized to an exec
      if(!ipaddr.matches("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$")) {
        Out.warning("Invalid IP address: " + ipaddr)
      } else {
        var p:Process = null
        var isr:InputStreamReader  = null
        var br:BufferedReader = null
        var pingresult = 0
        var pingcount = 0

        try {
            p = java.lang.Runtime.getRuntime().exec(pingcmd(ipaddr))
            isr = new InputStreamReader(p.getInputStream())
            br = new BufferedReader(isr)

            var read:String = br.readLine()

            do {
              // try to parse the ping result and extract the result. this will work as long as the time is enclosed between "time=" and "ms", which it should be both in windows and linux. YMMV.
              val indexTime:Int = read.indexOf("time=")

              if(indexTime >= 0) {
                val indexNumStart = indexTime + 5
                val indexNumEnd = read.indexOf("ms", indexNumStart)

                if(indexNumStart > 0 && indexNumEnd > 0) {
                  // parsing as double then casting, since linux gives a decimal number while windows doesn't
                  pingresult += read.substring(indexNumStart, indexNumEnd).trim().toDouble.toInt
                  pingcount += 1
                }
              }
              read = br.readLine()
            } while(read != null)

            if(pingcount > 0) {
              pingtime = pingresult / pingcount
            }
        } catch{case e: Exception =>
          Out.debug("Encountered exception " + e + " while trying to ping remote client")
        } finally {
          try { br.close();isr.close();p.destroy() } catch{case e:Exception => }
        }
      }

      if(pingtime > 0) {
        Out.debug("Approximate latency determined as ~" + pingtime + " ms")
      } else {
        Out.debug("Could not determine latency, conservatively guessing 20ms")
        pingtime = 20 // little to no compensation
      }

      val startTime = System.currentTimeMillis()

      if(downloadAndCacheFile(source, fileid)) {
        // this is mostly trial-and-error. we cut off 3 times the ping directly for TCP overhead (TCP three-way handshake + request/1st byte delay) , as well as cut off a factor of (1 second - pingtime) . this is capped to 200ms ping.
        val dlMillis = System.currentTimeMillis() - startTime
        pingtime = Math.min(200, pingtime)
        val dlTime = Math.max(0, ((dlMillis * (1.0 - pingtime / 1000.0) - pingtime * 3) / 1000.0))
        Out.debug("Clocked a download time of " + dlMillis + " ms. Ping delay fiddling reduced estimate to " + dlTime + " seconds.")
        return fileid + ":OK-" + dlTime
      }
    } catch{case e:Exception =>
      Out.warning("Encountered error " + e + " when doing proxy test against " + ipaddr + ":" + port + " on file " + fileid + ". Will not retry.")
    }

    fileid + ":FAIL-0"
  }

  // used by doProxyTest and downloadFilesFromServer
  private def downloadAndCacheFile(source:URL, fileid:String):Boolean = {
    if(HVFile.isValidHVFileid(fileid)) {
      val tmpfile = new File(CacheHandler.getTmpDir(), fileid)
      val ch = client.getCacheHandler()

      if(tmpfile.exists()) {
        tmpfile.delete()
      }

      val dler = new FileDownloader(source, 10000, 30000)

      if(dler.saveFile(tmpfile)) {
        val hvFile = HVFile.getHVFileFromFile(tmpfile, true)

        if(hvFile != null) {
          if(!hvFile.getLocalFileRef().exists()) {
            if(ch.moveFileToCacheDir(tmpfile, hvFile)) {
              ch.addFileToActiveCache(hvFile)
              Out.info("The file " + fileid + " was successfully downloaded and inserted into the active cache.")
            } else {
              Out.warning("Failed to insert " + fileid + " into cache.")
              tmpfile.delete()
              // failed to move, but didn't exist.. so we'll fail
              return false
            }
          } else {
            Out.info("The file " + fileid + " was successfully downloaded, but already exists in the cache.")
            tmpfile.delete()
          }

          // if the file was inserted, or if it exists, we'll call it a success
          Stats.fileRcvd()
          return true
        } else {
          Out.warning("Downloaded file " + fileid + " failed hash verification. Will not retry.")
        }
      } else {
        Out.warning("Failed downloading file " + fileid + " from " + source + ". Will not retry.")
      }

      if(tmpfile.exists()) {
        tmpfile.delete()
      }
    } else {
      Out.warning("Encountered invalid fileid " + fileid)
    }

    false
  }
}
