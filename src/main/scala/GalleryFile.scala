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

class GalleryFile(client:HentaiAtHomeClient, todir:File, fileid:String, gid:Int, page:Int, givenFilename:String) {

  private val hvfile = HVFile.getHVFileFromFileid(fileid)
  private var token:String = null

  private val fileExtIndex = givenFilename.lastIndexOf(".")
  private val filename = givenFilename.substring(0, Math.min(80, if(fileExtIndex > -1) fileExtIndex else givenFilename.length())) + "." + hvfile.getType()
  private val tofile = new File(todir, filename)

  private var tokentime:Long = 0 // tokens expire after about an hour, so we'll need to keep track of this in case a file repeatedly fails downloading
  private var state = GalleryFile.STATE_PENDING
  private var retrycount = 0
  private var lastretry:Long = 0

  Out.debug("Downloader: Pending download for " + fileid + " => " + filename)

  def setNewToken(token:String) {
    this.token = token
    this.tokentime = System.currentTimeMillis()
  }

  def getState() = state
  def getFileid() = fileid

  def attemptDownload():Int = {
    // wait at least six minutes between each download attempt, to allow the host routing cache to clear. increase retry delay by 6 minutes per fail.
    if(System.currentTimeMillis() < lastretry + 360000 * retrycount) {
      return GalleryFile.FILE_TEMPFAIL
    }
    // just in case, check for directory traversal
    if(! tofile.getParentFile().equals(todir)) {
      state = GalleryFile.STATE_PROCESSED_ERRORS
      return GalleryFile.FILE_PERMFAIL
    }
    // was the file already downloaded?
    if(tofile.isFile()) {
      Out.debug("Downloader: " + tofile + " already exists - marking as completed")
      state = GalleryFile.STATE_PROCESSED
      return GalleryFile.FILE_SUCCESS
    }

    val validToken = tokentime > System.currentTimeMillis() - 3600000		
    val fromfile = hvfile.getLocalFileRef()
    // try to download if it doesn't exist and we have a token
    if(validToken && !fromfile.isFile()) {
      Out.debug("Downloader: " + tofile + " - initializing GalleryFileDownloader")
      val gfd = new GalleryFileDownloader(client, fileid, token, gid, page, filename, retrycount > 3)
      gfd.initialize()
      var sleepTime = 1000
      var runTime = 0
      // we check every second, and give it max five minutes before moving on. if gfd finishes it after we give up, it will be caught on the next pass-through.
      do {
        try {
          Thread.sleep(sleepTime)
          runTime += sleepTime
        } catch{case e:java.lang.InterruptedException => }
      } while((gfd.getDownloadState() == GalleryFileDownloader.DOWNLOAD_PENDING) && (runTime < 300000))
    }

    if(fromfile.isFile()) {
      // copy the file to the output directory, and we're done
      FileTools.copy(fromfile, tofile)
      Out.debug("Downloader: " + fromfile + " copied to " + tofile)
      state = GalleryFile.STATE_PROCESSED
      GalleryFile.FILE_SUCCESS
    } else if(!validToken) {
      // we need a token for this file before we can download it
      GalleryFile.FILE_INVALID_TOKEN
    } else {
      // download was attempted but failed - flag necessary conditions for retry
      lastretry = System.currentTimeMillis()
      retrycount += 1
      if(retrycount > 100) {
        try {
          (new File(todir, filename + ".fail")).createNewFile()
          Out.debug("Downloader: Permanently failing download of " + fileid)
        } catch{case e:java.io.IOException => {
          Out.warning("Downloader: Failed to create empty .fail file")
          e.printStackTrace()
        }}
        state = GalleryFile.STATE_PROCESSED_ERRORS
        GalleryFile.FILE_PERMFAIL
      } else {
          GalleryFile.FILE_TEMPFAIL
      }
    }
  }
}

object GalleryFile {
  def FILE_SUCCESS = 1
  def FILE_TEMPFAIL = -1
  def FILE_PERMFAIL = -2
  def FILE_INVALID_TOKEN = -3

  def STATE_PROCESSED_ERRORS = -1
  def STATE_PENDING = 0
  def STATE_PROCESSED = 1

  def getGalleryFile(client:HentaiAtHomeClient , todir:File, fileid:String, gid:Int, page:Int, filename:String): GalleryFile = {
    if((client != null ) && (todir != null) && (fileid != null) && (gid > 0) && (page > 0) && (filename != null)) {
        if(HVFile.isValidHVFileid(fileid) && (filename.length() > 0)) {
            return new GalleryFile(client, todir, fileid, gid, page, filename)
        }
    }
    Out.warning("Invalid GalleryFile " + fileid + " (" + filename + ")")
    return null
  }
}
