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

/*

- Some inefficient code in the random data generation used for speedtests was replaced with a much faster (10-12x) version. This should cause less CPU spikes during speed testing, and potentially higher tested speeds if the CPU was capped. (Thanks Hobbitmon)

- With a large cache and under heavy load, having to free disk space would sometimes take a long time and therefore prevent the client from doing anything else that runs from the main thread or requires database access. Some tweaks to the free space generator should now prevent this.

- Made log flushing on program shutdown more consistent.


http://stackoverflow.com/questions/4507572/swing-jtextarea-multithreading-problem-interruptedexception

[b]To update an existing client: shut it down, download [url=http://hentaiathome.net/get/HentaiAtHome_1.2.4.zip]Hentai@Home 1.2.4[/url], extract the archive, copy the jar files over the existing ones, then restart the client.[/b]

[b]For information on how to join Hentai@Home, check out [url=http://forums.e-hentai.org/index.php?showtopic=19795]The Hentai@Home Project FAQ[/url].[/b]

[b]Other download options can be found at [url=http://g.e-hentai.org/hentaiathome.php]the usual place[/url].[/b]

*/

package org.hath.base

import java.io.File
import java.lang.Thread

class HentaiAtHomeClient(iqh:InputQueryHandler, args:Array[String]) extends Runnable {
  //private InputQueryHandler iqh
  //private Out out
  private var shutdownHook:ShutdownHook = null
  private var isShutdown = false
  private var reportShutdown = false
  private var fastShutdown = false
  private var httpServer:HTTPServer  = null
  private var clientAPI:ClientAPI = null
  private var cacheHandler:CacheHandler = null
  private var serverHandler:ServerHandler = null
  private var galleryDownloadManager:GalleryDownloadManager = null

  private val myThread = new Thread(this)
  myThread.start()

  private var threadSkipCounter =0
  private var suspendedUntil:Long = 0
  //private String[] args

  // master thread for all regularly scheduled tasks
  // note that this function also does most of the program initialization, so that the GUI thread doesn't get locked up doing this when the program is launched through the GUI extension.
  def run() {
    //out = [>new<] Out()
    Out.overrideDefaultOutput()
    Out.info("Hentai@Home " + Settings.CLIENT_VERSION + " starting up")
    Out.info("")
    Out.info("Copyright (c) 2008-2014, E-Hentai.org - all rights reserved.")
    Out.info("This software comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to modify and redistribute it under the GPL v3 license.")
    Out.info("")

    val sqlitejdbc = "sqlite-jdbc-3.7.2.jar"
    if(! (new File(sqlitejdbc)).canRead()) {
      Out.error("Required library file " + sqlitejdbc + " could not be found. Please make sure you are starting the program from the H@H directory, and that all H@H files are present.")
      System.exit(-1)
    }

    System.setProperty("http.keepAlive", "false")

    Settings.setActiveClient(this)
    Settings.parseArgs(args)

    Stats.resetStats()
    Stats.setProgramStatus("Logging in to main server...")

    // processes commands from the server and interfacing code (like a GUI layer)
    clientAPI = new ClientAPI(this)

    if(Settings.loadClientLoginFromFile()) {
        Out.info("Loaded login settings from " + Settings.DATA_FILENAME_CLIENT_LOGIN)
    }

    if(!Settings.loginCredentialsAreSyntaxValid()) {
        Settings.promptForIDAndKey(iqh)
    }

    // handles notifications other communication with the hentai@home server
    serverHandler = new ServerHandler(this)
    serverHandler.loadClientSettingsFromServer()

    Stats.setProgramStatus("Initializing cache handler...")

    // manages the files in the cache
    try {
      cacheHandler = new CacheHandler(this)
      cacheHandler.initializeCacheHandler()
      cacheHandler.flushRecentlyAccessed()
    } catch{case ioe:java.io.IOException => {
      setFastShutdown()
      HentaiAtHomeClient.dieWithError(ioe)
      return
    }}

    Out.info("Calculating initial cache list file size...")
    cacheHandler.calculateStartupCachedFilesStrlen()
    Out.info("Calculated cacheListStrlen = " + cacheHandler.getStartupCachedFilesStrlen())

    Stats.setProgramStatus("Starting HTTP server...")

    // handles HTTP connections used to request images and receive commands from the server
    httpServer = new HTTPServer(this)
    if(!httpServer.startConnectionListener(Settings.getClientPort())) {
        setFastShutdown()
        HentaiAtHomeClient.dieWithError("Failed to initialize HTTPServer")
        return
    }

    Stats.setProgramStatus("Sending startup notification...")

    Out.info("Notifying the server that we have finished starting up the client...")
    if(!serverHandler.notifyStart()) {
      setFastShutdown()
      Out.info("Startup notification failed.")
      return
    }

    httpServer.allowNormalConnections()

    reportShutdown = true
    shutdownHook = new ShutdownHook()
    java.lang.Runtime.getRuntime().addShutdownHook(shutdownHook)

    if(Settings.isWarnNewClient()) {
      val newClientWarning = "A new client version is available. Please download it from http://hentaiathome.net/ at your convenience."
      Out.warning(newClientWarning)

      if(Settings.getActiveGUI() != null) {
        Settings.getActiveGUI().notifyWarning("New Version Available", newClientWarning)
      }
    }

    if(cacheHandler.getCacheCount() < 1) {
      Out.info("Important: Your cache does not yet contain any files. Because of this, you won't receive much traffic until the client has downloaded some files. This should usually happen within a few minutes. The longer you run the client, the more files it will download to your cache, which directly translates into higher utilization.")
    }

    // check if we're in an active schedule
    serverHandler.refreshServerSettings()

    Out.info("Activated.")
    Stats.resetBytesSentHistory()
    Stats.programStarted()

    cacheHandler.processBlacklist(259200, false)

    galleryDownloadManager = new GalleryDownloadManager(this)

    suspendedUntil = 0
    threadSkipCounter = 1

    var lastThreadTime:Long = 0

    System.gc()

    while(!isShutdown) {
      try {
        //myThread.sleep(Math.max(1000, 10000 - lastThreadTime))
        Thread.sleep(Math.max(1000, 10000 - lastThreadTime))
      } catch{case e:java.lang.InterruptedException => {
        Out.debug("Master thread sleep interrupted")
      }}

      val startTime = System.currentTimeMillis()

      if(!isShutdown && suspendedUntil < System.currentTimeMillis()) {
        Stats.setProgramStatus("Running")

        if(suspendedUntil > 0) {
          resumeMasterThread()
        }
        if(threadSkipCounter % 30 == 0) {
          serverHandler.stillAliveTest()
        }
        if(threadSkipCounter % 6 == 2) {
          httpServer.pruneFloodControlTable()
        }
        if(threadSkipCounter % 30 == 15) {
          if( (System.currentTimeMillis() / 1000).toInt - Stats.getLastServerContact() < 360 ) {
            cacheHandler.pruneOldFiles()
          }
        }
        if(threadSkipCounter % 2160 == 2159) {
          cacheHandler.processBlacklist(43200, false)
        }
        cacheHandler.flushRecentlyAccessed()
        httpServer.nukeOldConnections(false)
        Stats.shiftBytesSentHistory()

        threadSkipCounter += 1
      }
      lastThreadTime = System.currentTimeMillis() - startTime
    }
  }

  def isSuspended() = suspendedUntil > System.currentTimeMillis()

  def suspendMasterThread(suspendTime:Int) =
    if(suspendTime > 0 && suspendTime <= 86400 && suspendedUntil < System.currentTimeMillis()) {
      Stats.programSuspended()
      val suspendTimeMillis = suspendTime * 1000
      suspendedUntil = System.currentTimeMillis() + suspendTimeMillis
      Out.debug("Master thread suppressed for " + (suspendTimeMillis / 1000) + " seconds.")
      serverHandler.notifySuspend()
    } else false

  def resumeMasterThread() = {
    suspendedUntil = 0
    threadSkipCounter = 0
    Stats.programResumed()
    serverHandler.notifyResume()
  }

  def getInputQueryHandler() = iqh
  def getHTTPServer() = httpServer
  def getCacheHandler() = cacheHandler
  def getServerHandler() = serverHandler
  def getClientAPI() = clientAPI

  def setFastShutdown() {
    Out.flushLogs()
    fastShutdown = true
  }

  def shutdown():Unit = shutdown(false, null)

  private def shutdown(error:String):Unit = shutdown(false, error)
  private def shutdown(fromShutdownHook:Boolean, shutdownErrorMessage:String):Unit = {
    Out.flushLogs()

    if(!isShutdown) {
      isShutdown = true
      Out.info("Shutting down...")
      if(reportShutdown && serverHandler != null) {
        serverHandler.notifyShutdown()
      }
      if(!fastShutdown && httpServer != null) {
        httpServer.stopConnectionListener()
        Out.info("Shutdown in progress - please wait 25 seconds")
        try {
          Thread.sleep(25000)
        } catch{case e: java.lang.InterruptedException => }
        if(Stats.getOpenConnections() > 0) {
          httpServer.nukeOldConnections(true)
          Out.info("All connections cleared.")
        }
      }
      if(cacheHandler != null) {
        cacheHandler.flushRecentlyAccessed()
        cacheHandler.terminateDatabase()
      }
      if(myThread != null) {
        myThread.interrupt()
      }

      if(Math.random() > 0.99) {
        Out.info(
"                             .,---.\n" +
"                           ,/XM#MMMX,\n" +
"                         -%##########M%,\n" +
"                        -@######%  $###@=\n" +
"         .,--,         -H#######$   $###M:\n" +
"      ,$M###MMX     .##########$HM###X=\n" +
"    ,/@##########H=      ################+\n" +
"   -+#############M/,      %##############+\n" +
"   %M###############=      /##############:\n" +
"   H################      .M#############.\n" +
"   @###############M      ,@###########M:.\n" +
"   X################,      -$=X#######@:\n" +
"   /@##################%-     +######$-\n" +
"   .##################X     .X#####+,\n" +
"    .H################/     -X####+.\n" +
"      ,X##############,       .MM/\n" +
"         ,:+$H@M#######M#$-    .$$=\n" +
"              .,-=+$@###X:    /=.\n" +
"                     .,/X$   .::,\n" +
"                         .,    ..    \n"
)
      } else {
        val sd = List("I don't hate you", "Whyyyyyyyy...", "No hard feelings", "Your business is appreciated", "Good-night")
        Out.info(sd(Math.floor(Math.random() * sd.length).toInt))
      }
      if(shutdownErrorMessage != null) {
        if(Settings.getActiveGUI() != null) {
          Settings.getActiveGUI().notifyError(shutdownErrorMessage)
        }
      }
      Out.disableLogging()
    }

    if(!fromShutdownHook) {
      System.exit(0)
    }
  }

  def isShuttingDown() = isShutdown

  private class ShutdownHook extends Thread {
    override def run() {
      shutdown(true, null)
    }
  }
}

object HentaiAtHomeClient {
  // static crap

  def dieWithError(e:Exception) {
    e.printStackTrace()
    dieWithError(e.toString())
  }

  def dieWithError(error:String) {
    Out.error("Critical Error: " + error)
    Stats.setProgramStatus("Died")
    Settings.getActiveClient().shutdown(false, error)
  }

  def main(args:Array[String]) {
    try {
      val iqh = InputQueryHandlerCLI.getIQHCLI()
      new HentaiAtHomeClient(iqh, args)
    } catch{ case e:Exception =>
      Out.error("Failed to initialize InputQueryHandler")
    }
  }
}
