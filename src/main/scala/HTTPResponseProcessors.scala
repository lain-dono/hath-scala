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

import java.io.File
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.nio.charset.Charset
import java.util.Arrays
import java.util.Random
import java.lang.Thread

import scala.collection.JavaConversions._
import scala.util.control.Breaks._


abstract class HTTPResponseProcessor {
  def initialize() = 0
  def cleanup() {}

  def getContentType() = Settings.CONTENT_TYPE_DEFAULT
  def getContentLength():Int

  def getBytes():Array[Byte] //throws Exception
  def getBytesRange(len: Int):Array[Byte] //throws Exception

  private var header = ""
  def getHeader() = header
  def addHeaderField(name:String, value:String):Unit = {
    // TODO: encode the value if needed.
    header += name + ": " + value + "\r\n"
  }
}



class HTTPResponseProcessorCachelist(cacheHandler:CacheHandler) extends HTTPResponseProcessor {
  private var cacheListWritten = 0
  private var segmentIndex = 0
  private var segmentCount = 0
  private var fileidBuffer: StringBuilder = null

  override def initialize() = {
    // note: this class is only safe to use during startup while the client is still single-threaded
    // any cache additions or deletions between the initial file length is calculated and this class is invoked will make things fail

    cacheListWritten = 0
    segmentIndex = 0
    segmentCount = cacheHandler.getSegmentCount()

    fileidBuffer = new StringBuilder(Settings.TCP_PACKET_SIZE_HIGH + Math.round(2 * cacheHandler.getStartupCachedFilesStrlen() / segmentCount))

    Out.info("Sending cache list, and waiting for the server to register the cached files.. (this could take a while)")

    200
  }

  def getContentLength() = cacheHandler.getStartupCachedFilesStrlen()
  def getBytes() = getBytesRange(cacheHandler.getStartupCachedFilesStrlen())

  def getBytesRange(len:Int): Array[Byte] = {
    //Out.debug("Before: cacheListWritten=" + cacheListWritten + ", fileidBuffer=" + fileidBuffer.length() +", len=" + len)
    while( fileidBuffer.length() < len ) {
      breakable {
        Out.info("Retrieving segment " + segmentIndex + " of " + segmentCount)
        if( segmentIndex >= segmentCount ) {
          HentaiAtHomeClient.dieWithError("Segment out of range")
        }
        var fileList: Iterable[String] = cacheHandler.getCachedFilesSegment(Integer.toHexString(segmentCount | segmentIndex).substring(1))
        segmentIndex += 1

        if( fileList.size() < 1 ) {
          break
        }
        fileList.foreach((fileid: String) => {
          fileidBuffer.append(fileid + "\n")
        })
      }
    }

    var returnBytes = fileidBuffer.substring(0, len).getBytes(java.nio.charset.Charset.forName("ISO-8859-1"))
    fileidBuffer.delete(0, len)

    if(returnBytes.length != len) {
        HentaiAtHomeClient.dieWithError("Length of cache list buffer (" + returnBytes.length + ") does not match requested length (" + len + ")! Bad program!")
    }

    cacheListWritten += returnBytes.length
    //Out.debug("After: cacheListWritten=" + cacheListWritten + ", fileidBuffer=" + fileidBuffer.length() +", len=" + len)
    returnBytes
  }
}

// this class provides provides a buffered interface to read a file in chunks
class HTTPResponseProcessorFile(requestedHVFile: HVFile) extends HTTPResponseProcessor {
  //private HVFile requestedHVFile
  private var bis: BufferedInputStream = null
  private var off = 0

  override def initialize() = {
    var responseStatusCode = 0

    val file = requestedHVFile.getLocalFileRef()

    try {
      bis = new BufferedInputStream(new FileInputStream(file), if (Settings.isUseLessMemory())  8192 else 65536)
      responseStatusCode = 200
      Stats.fileSent()
    } catch {
      case e: java.io.IOException => {
        Out.warning("Failed reading content from " + file)
        responseStatusCode = 500
      }
    }

    responseStatusCode
  }

  override def cleanup() {
    if(bis != null) {
      try {
        bis.close()
      } catch {
        case e: Exception => e.printStackTrace()
      }
    }
  }

  override def getContentType() = requestedHVFile.getMimeType()
  def getContentLength() = if(bis == null) 0 else requestedHVFile.getSize()

  def getBytes() = getBytesRange(requestedHVFile.getSize())

  def getBytesRange(len:Int):Array[Byte] = {
    var range: Array[Byte] = null

    try {
      range = new Array[Byte](len)
      bis.read(range)
    } catch {
      case e: Exception => e.printStackTrace()
    }

    off += len
    range
  }
}


class HTTPResponseProcessorProxy(session:HTTPSession, fileid:String, token:String, gid:Int, page:Int, filename:String) extends HTTPResponseProcessor {
  private val gdf = new GalleryFileDownloader(session.getHTTPServer().getHentaiAtHomeClient(), fileid, token, gid, page, filename, false)
  private var readoff = 0

  override def initialize() = {
    Out.info(session + ": Initializing proxy request...")
    gdf.initialize()
  }

  override def getContentType()   = gdf.getContentType()
  def getContentLength() = gdf.getContentLength()

  def getBytes() = getBytesRange(getContentLength())

  def getBytesRange(len:Int): Array[Byte] =  {
    // wait for data
    val endoff = readoff + len

    //Out.debug("Reading data with readoff=" + readoff + " len=" + len + " writeoff=" + writeoff)

    var timeout = 0

    while(endoff > gdf.getCurrentWriteoff()) {
      try {
        //Thread.currentThread().sleep(10)
        Thread.sleep(10)
      } catch {
        case e: Exception =>
      }

      timeout += 1
      if( timeout > 30000 ) {
        // we have waited about five minutes, probably won't happen
        Out.info("Timeout while waiting for proxy request.")
        throw new Exception("Timeout while waiting for proxy request.")
      }
    }

    val range = gdf.getDownloadBufferRange(readoff, endoff)
    readoff += len
    range
  }
}

class HTTPResponseProcessorSpeedtest(testsize: Int) extends HTTPResponseProcessor {
  val rand = new Random()

  def getContentLength() = testsize
  def getBytes() = getRandomBytes(testsize)
  def getBytesRange(len: Int) = getRandomBytes(len)
  private def getRandomBytes(len: Int): Array[Byte] = {
    // generate a random body the server can use to gauge the actual upload speed capabilities of this client
    var random = new Array[Byte](len)
    rand.nextBytes(random)
    random
  }
}

class HTTPResponseProcessorText(responseBody: String, mimeType: String, charset: Charset) extends HTTPResponseProcessor {
  if(responseBody.length() > 0) {
      Out.debug("Response Written:")
      if(responseBody.length() < 10000) {
          Out.debug(responseBody)
      } else {
          Out.debug("tldw")
      }
  }

  private val responseBytes = responseBody.getBytes(charset)
  private var off = 0
  private val contentType = mimeType + " charset=" + charset.name()

  def this(responseBody: String)                   = this(responseBody, "text/html", Charset.forName("ISO-8859-1"))
  def this(responseBody: String, mimeType: String) = this(responseBody, mimeType, Charset.forName("ISO-8859-1"))

  override def getContentType() = this.contentType
  def getContentLength() = if(responseBytes == null) 0 else responseBytes.length
  def getBytes() = responseBytes

  def getBytesRange(len: Int): Array[Byte] = {
    val range = Arrays.copyOfRange(responseBytes, off, Math.min(responseBytes.length, off + len))
    off += len
    range
  }
}

