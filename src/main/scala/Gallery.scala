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
//import java.util.List

import scala.collection.JavaConversions._
import scala.util.control.Breaks._

class Gallery(client:HentaiAtHomeClient, hhdlFile:File, todir:File, title:String, information:String, galleryFiles:Array[GalleryFile]) {
  private var state = Gallery.STATE_PENDING

  Out.debug("Downloader: Download session for gallery " + title + " is now pending")
  def getState() = state

  def isNotPending() = state != Gallery.STATE_PENDING

  // takes and modifies list of the files that needs a token - these are borked together and requested by the main thread
  def galleryPass(_requestTokens:List[GalleryFile]):List[GalleryFile] = {
    var allFilesProcessed = true
    var errorsEncountered = false

    var requestTokens = _requestTokens

    for(gf <- galleryFiles) { breakable{
      if(!client.isShuttingDown()) {
        if(gf == null) {
          errorsEncountered = true
          break // continue
        }

        gf.getState() match {
          case Gallery.STATE_PROCESSED_ERRORS => errorsEncountered = true
          case Gallery.STATE_PENDING => {
            allFilesProcessed = false
            if(gf.attemptDownload() == GalleryFile.FILE_INVALID_TOKEN) {
              if(! requestTokens.contains(gf) && requestTokens.size() < 20) {
                requestTokens :+= (gf)
              }
            }
          }
        }
      }
    }}

    if(allFilesProcessed) {
      if(errorsEncountered) {
        Out.info("Downloader: Finished downloading gallery " + title + ", but some files could not be retrieved")
        state = Gallery.STATE_PROCESSED_ERRORS
      } else {
        Out.info("Downloader: Finished downloading gallery " + title)
        state = Gallery.STATE_PROCESSED
      }

      try {
        FileTools.putStringFileContentsUTF8(new File(todir, "galleryinfo.txt"), information)
      } catch{case e:java.io.IOException=> {
        Out.warning("Downloader: Could not write galleryinfo file")
        e.printStackTrace()
      }}

      hhdlFile.delete()
    }

    return requestTokens
  }
}

object Gallery {
  val STATE_PROCESSED_ERRORS = -1
  val STATE_PENDING = 0
  val STATE_PROCESSED = 1
}
