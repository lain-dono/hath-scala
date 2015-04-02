/*

Copyright 2008-2013 E-Hentai.org
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

import java.net._
import java.io._

import scala.util.control.Breaks._

class FileDownloader(source:URL, timeout:Int, maxDLTime:Int) extends Runnable {
  private var retries = 3
  private var timeDownloadStart:Long = 0
  private var timeFirstByte:Long = 0
  private var timeDownloadFinish:Long = 0
  private var bytearray:Array[Byte] = null
  private var myThread:Thread = null
  private var downloadLock:Object = new Object()
  private var started = false

  def this(source: URL)              = this(source, 30000, Int.MaxValue)
  def this(source: URL, timeout:Int) = this(source, timeout, Int.MaxValue)

  def saveFile(destination:File):Boolean = {
    if(destination.exists()) {
      destination.delete()
    }

    var fos:FileOutputStream = null

    try {
      FileTools.checkAndCreateDir(destination.getParentFile())

      if(downloadFile()) {
        fos = new FileOutputStream(destination)
        fos.write(bytearray, 0, bytearray.length)
        fos.close()

        return true
      }
    } catch{ case e: Exception => {
      try { fos.close() } catch{ case e2:Exception => } // nuke file handle if open

      if(e.isInstanceOf[java.io.IOException] && e.getMessage().equals("There is not enough space on the disk")) {
        Out.warning("Error: No space on disk")
      } else {
        Out.warning(e + " while saving file " + source + " to " + destination.getAbsolutePath())
        e.printStackTrace()
      }
    }}

    return false
  }

  def getTextContent() =
    if(downloadFile()) {
      new String(bytearray, 0, bytearray.length)
    } else null

  private def downloadFile():Boolean = {
    // this will block while the file is downloaded
    if(myThread == null) {
      // if startAsyncDownload has not been called, we invoke run() directly and skip threading
      run()
    } else waitAsyncDownload()
    timeDownloadFinish > 0
  }

  // start a new thread to handle the download. this will return immediately
  def startAsyncDownload() {
    if(myThread == null) {
      myThread = new Thread(this)
      myThread.start()
    }
  }

  def waitAsyncDownload() = {
      // synchronize on the download lock to wait for the download attempts to complete before returning
      downloadLock.synchronized {}
      timeDownloadFinish > 0
  }

  def getDownloadTimeMillis():Long =
    if (timeFirstByte > 0)  timeDownloadFinish - timeFirstByte else 0

  def run() {
    downloadLock.synchronized {
      if(started) return
      started = true
      breakable { while(retries > 0) {
        retries -= 1
        var is:InputStream = null
        var bis:BufferedInputStream = null

        try {
          Out.info("Connecting to " + source.getHost() + "...")

          val connection = source.openConnection()
          connection.setConnectTimeout(10000)
          connection.setReadTimeout(timeout) // this doesn't always seem to work however, so we'll do it somewhat differently..
          connection.setRequestProperty("Connection", "Close")
          connection.setRequestProperty("User-Agent", "Mozilla/5.0 (Windows U Windows NT 5.1 en-US rv:1.8.1.12) Gecko/20080201 Firefox/2.0.0.12")
          connection.connect()

          Out.debug("Connected to " + source)
          val contentLength = connection.getContentLength()

          if(contentLength < 0) {
            // H@H note: since we control all systems in this case, we'll demand that clients and servers always send the Content-Length
            Out.warning("Remote host did not send Content-Length, aborting transfer.")
            return
          } else if(contentLength > 10485760) {
            // H@H note: we don't want clients trying to provoke an outofmemory exception by returning a malformed oversized reply,
            // so we'll limit the download size to the H@H max (10 MB). the server will never send anything this large as a response either.
            Out.warning("Reported contentLength " + contentLength + " on request " + source + " is out of bounds!")
            return
          }

          Out.debug("Received contentLength=" + contentLength)

          bytearray = new Array[Byte](contentLength)
          is = connection.getInputStream()
          bis = new BufferedInputStream(is)

          Out.info(source.getPath() + (if(source.getQuery() == null) "" else "?" + source.getQuery()) + ": Retrieving " + contentLength + " bytes...")
          timeDownloadStart = System.currentTimeMillis()

          var bytecounter = 0 // counts the number of bytes read
          var time = 0        // counts the approximate time (in nanofortnights) since last byte was received

          while(bytecounter < contentLength) {
            val available = bis.available()

            if(available > 0) {
              // read-data loop..

              if(timeFirstByte == 0) {
                timeFirstByte = System.currentTimeMillis()
              }

              time = 0
              val readcount = bis.read(bytearray, bytecounter, available)

              if(readcount >= 0) {
                bytecounter += readcount
              } else {
                // readcount == -1 => EOF
                Out.warning("\nServer sent premature EOF, aborting.. (" + bytecounter + " of " + contentLength + " bytes received)")
                throw new java.net.SocketException("Unexpected end of file from server")
              }
            } else {
              // wait-for-data loop...

              if(System.currentTimeMillis() - timeDownloadStart > maxDLTime) {
                Out.warning("\nDownload time limit has expired, aborting...")
                throw new java.net.SocketTimeoutException("Download timed out")							
              } else if(time > timeout) {
                Out.warning("\nTimeout detected waiting for byte " + bytecounter + ", aborting..")
                throw new java.net.SocketTimeoutException("Read timed out")
              }

              time += 5
              //Thread.currentThread().sleep(5)
              Thread.sleep(5)
            }
          }

          Out.debug("Finished. bytecounter=" + bytecounter)

          bis.close()
          is.close()

          Stats.bytesRcvd(contentLength)

          timeDownloadFinish = System.currentTimeMillis()
          return
        } catch{ case e:Exception => {
          try { bis.close() } catch{case e2:Exception =>{}}
          try { is.close() } catch{case e3:Exception =>{}}

          if(printErr(e)) break
        }}
      }}
    }

    Out.warning("Exhaused retries or aborted getting " + source)
    return
  }

  private def printErr(e:Exception):Boolean = {
    val message = e.getMessage()
    val cause = e.getCause()
    var causemessage:String = null

    if(cause != null) {
      causemessage = if(cause.getMessage() != null) cause.getMessage() else ""
    }

    if(message != null) {
      if(message.equals("Connection timed out: connect")) {
        Out.warning("Connection timed out getting " + source + ", retrying.. (" + retries + " tries left)")
      } else if(message.equals("Connection refused: connect")) {
        Out.warning("Connection refused getting " + source + ", retrying.. (" + retries + " tries left)")
      } else if(message.equals("Unexpected end of file from server")) {
        Out.warning("Connection prematurely reset getting " + source + ", retrying.. (" + retries + " tries left)")
      } else if(e.isInstanceOf[java.io.FileNotFoundException]) {
        Out.warning("Server returned: 404 Not Found")
        return true
      } else if(message.indexOf("403 for URL") >= 0) {
        Out.warning("Server returned: 403 Forbidden")
        return true
      } else if(e.isInstanceOf[java.net.SocketException] && message.equals("Connection reset")) {
        Out.warning("Connection reset getting " + source + ", retrying.. (" + retries + " tries left)")
      } else if(e.isInstanceOf[java.net.UnknownHostException]) {
        Out.warning("Unknown host " + source.getHost() + ", aborting..")
        return true
      } else if(e.isInstanceOf[java.net.SocketTimeoutException]) {
        Out.warning("Read timed out, retrying.. (" + retries + " tries left)")
      } else {
        Out.warning("Unhandled exception: " + e.toString())
        e.printStackTrace()
        Out.warning("Retrying.. (" + retries + " tries left)")
      }
    } else if(cause != null){
      if(cause.isInstanceOf[java.io.FileNotFoundException]) {
        Out.warning("Server returned: 404 Not Found")
        return true
      } else if(causemessage.indexOf("403 for URL") >= 0) {
        Out.warning("Server returned: 403 Forbidden")
        return true
      } else if(causemessage.equals("Unexpected end of file from server")) {
        Out.warning("Connection prematurely reset getting " + source + ", retrying.. (" + retries + " tries left)")
      } else {
        Out.warning("Unhandled exception/cause: " + e.toString())
        e.printStackTrace()
        Out.warning("Retrying.. (" + retries + " tries left)")
      }
    } else {
      Out.warning("Exception with no exception message nor cause:")
      e.printStackTrace()
      Out.warning("Retrying.. (" + retries + " tries left)")
    }
    return false
  }

}

object FileDownloader {
  def main(args: Array[String]) {
    if(args.length != 2) {
      System.out.println("Need source and destination.")
    } else {
      try {
        val source = new URL(args(0))
        val dest = new File(args(1))

        System.out.println("Downloading file " + source)

        val dler = new FileDownloader(source)
        dler.saveFile(dest)
      } catch { case e: Exception => e.printStackTrace() }
    }
  }
}
