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

// convenience class for the GUI

//import java.util.List
//import java.util.ArrayList
//

trait StatListener extends java.util.EventListener {
  def statChanged(stat:String):Unit
}

object Stats {
  private var statListeners: Set[StatListener] = Set()

  private var clientRunning = false
  private var clientSuspended = false
  private var programStatus:String = null
  private var clientStartTime:Long = 0
  private var filesSent = 0
  private var filesRcvd = 0
  private var bytesSent:Long = 0
  private var bytesRcvd:Long = 0
  private var cacheCount = 0
  private var cacheSize:Long = 0
  private var bytesSentHistory = Array.fill(361)(0) /*List()*/ //new Array[Int](361)
  private var openConnections = 0
  private var lastServerContact = 0

  private var currentByteSendSpeed = new Array[Int](2)
  private var currentByteSendFlip = 0

  resetStats()
  //for(int i=0 i<361 i++) {
  //	bytesSentHistory[i] = (int) (Math.random() * 1000000)
  //}

  def addStatListener(listener:StatListener) {
    statListeners.synchronized {
      //if(!statListeners.contains(listener)) {
        statListeners += listener
      //}
    }
  }

  def removeStatListener(listener:StatListener) {
    statListeners.synchronized {
      statListeners = statListeners diff Set(listener)
    }
  }

  private def statChanged(stat:String) {
    val client = Settings.getActiveClient()
    var announce = client == null || !client.isShuttingDown()

    if(announce) {
      statListeners.synchronized {
        statListeners.foreach(_.statChanged(stat))
      }
    }
  }

  // modify methods
  def setProgramStatus(newStatus:String) {
    programStatus = newStatus
    statChanged("programStatus")
  }

  def resetStats() {
    clientRunning = false
    programStatus = "Stopped"
    clientStartTime = 0
    lastServerContact = 0
    filesSent = 0
    filesRcvd = 0
    bytesSent = 0
    bytesRcvd = 0
    cacheCount = 0
    cacheSize = 0
    resetBytesSentHistory()

    statChanged("reset")
  }

  // run this from a thread every 10 seconds
  def shiftBytesSentHistory() {
    //bytesSentHistory = 0 :: bytesSentHistory.dropRight(1)
    bytesSentHistory = Array[Int](1) ++ bytesSentHistory.slice(1, bytesSentHistory.size - 1)
    statChanged("bytesSentHistory")
  }

  def resetBytesSentHistory() {
    bytesSentHistory = Array.fill(bytesSentHistory.size)(0)
    statChanged("bytesSentHistory")
  }

  def programStarted() {
    clientStartTime = System.currentTimeMillis()
    clientRunning = true
    setProgramStatus("Running")
    statChanged("clientRunning")
  }

  def programSuspended() {
    clientSuspended = true
    setProgramStatus("Suspended")
    statChanged("clientSuspended")
  }

  def programResumed() {
    clientSuspended = false
    setProgramStatus("Running")
    statChanged("clientSuspended")
  }

  def serverContact() {
    lastServerContact = (System.currentTimeMillis() / 1000).toInt
    statChanged("lastServerContact")
  }

  def fileSent() {
    filesSent += 1
    statChanged("fileSent")
  }

  def fileRcvd() {
    filesRcvd += 1
    statChanged("fileRcvd")
  }

  private def checkCurrentTimeSendSpeed():Int = {
    val currentTime = (System.currentTimeMillis() / 1000).toInt
    if(currentByteSendFlip < currentTime) {
      currentByteSendSpeed(currentTime % 2) = 0
      if(currentByteSendFlip + 1 < currentTime) {
        currentByteSendSpeed((currentTime + 1) % 2) = 0
      }
    }
    currentByteSendFlip = currentTime
    currentTime
  }

  def bytesSent(b:Int) {
    if(clientRunning) {
      bytesSent += b
      bytesSentHistory(0) += b.toInt
    }
    currentByteSendSpeed(checkCurrentTimeSendSpeed() % 2) += b
    statChanged("bytesSent")
  }

  def bytesRcvd(b:Int) {
    if(clientRunning) {
      bytesRcvd += b
      statChanged("bytesRcvd")
    }
  }

  def setCacheCount(count:Int) {
    cacheCount = count
    statChanged("cacheCount")
  }
  def setCacheSize(size:Long) {
    cacheSize = size
    statChanged("cacheSize")
  }
  def setOpenConnections(conns:Int) {
    openConnections = conns
    statChanged("openConnections")
  }


  //
  // accessor methods
  //
  def isClientRunning() = clientRunning
  def isClientSuspended() = clientSuspended
  def getProgramStatus() = programStatus
  def getUptime() = getUptimeDouble().toInt
  def getUptimeDouble():Double = {
    if(clientRunning)
      ((System.currentTimeMillis() - clientStartTime) / 1000.0)
    else 0
  }
  def getFilesSent() = filesSent
  def getFilesRcvd() = filesRcvd
  def getBytesSent() = bytesSent
  def getBytesSentHistory() = bytesSentHistory
  def getBytesRcvd() = bytesRcvd
  def getBytesSentPerSec() = {
    val uptime = getUptimeDouble()
    if(uptime > 0) (bytesSent / uptime).toInt else 0
  }
  def getBytesRcvdPerSec() = {
    val uptime = getUptimeDouble()
    if(uptime > 0)  (bytesRcvd / uptime).toInt else 0
  }
  def getCurrentBytesPerSec() = currentByteSendSpeed((checkCurrentTimeSendSpeed() + 1) % 2)
  def getCacheCount() = cacheCount
  def getCacheSize() = cacheSize
  def getCacheFree() = Settings.getDiskLimitBytes() - cacheSize
  def getCacheFill():Float =
    if(Settings.getDiskLimitBytes() != 0)  cacheSize / Settings.getDiskLimitBytes().toFloat
    else 0
  def getOpenConnections() = openConnections
  def getLastServerContact() = lastServerContact
}
