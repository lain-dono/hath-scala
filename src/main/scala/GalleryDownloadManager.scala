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
// TODO
// /*
import java.lang.Thread
//import java.util.List
//import java.util.ArrayList
import java.util.Hashtable
import java.io.File

import scala.util.control.Breaks._
import scala.collection.JavaConversions._

class GalleryDownloadManager(client: HentaiAtHomeClient) extends Runnable {

  private var hhdldir:File = null
  private var downloadeddir:File = null

  try {
    hhdldir = FileTools.checkAndCreateDir(new File("hathdl"))
    downloadeddir = FileTools.checkAndCreateDir(new File("downloaded"))
  } catch{ case e:java.io.IOException =>
    HentaiAtHomeClient.dieWithError(e)
  }

  // this is never cleared while the client is running
  private var processedHHDLFiles = new java.util.ArrayList[File](0)
  private var pendingGalleries:List[Gallery] = List()

    // we wait until one hour after client start before we start requesting tokens, so the server has time to qualify the client. there's no point reducing this, as it's checked server-side.
  private var lastTokenRequest = System.currentTimeMillis() + 3600060

  private val myThread = new Thread(this)
  myThread.start()

  private var sleepTime:Long = 0

  def run() {
    while(!client.isShuttingDown()) {
        sleepTime = 10000
        if(client.isSuspended()) {
          lastTokenRequest = System.currentTimeMillis() + 960000 // again waiting until 15 minutes after a suspend
          sleepTime = 60000
        } else {
          process()
        }
        try {
            //myThread.sleep(sleepTime)
            Thread.sleep(sleepTime)
        } catch{case e:java.lang.InterruptedException => }
    }

    Out.info("Gallery Download Manager terminated.")
  }

  def process() {
    val process = hhdldir.listFiles()
      .filter(_.isFile())
      .filter(_.getName().endsWith(".hathdl"))

    try {
      for(processFile <- process) {
        if(! processedHHDLFiles.contains(processFile)) {
          Out.debug("Downloader: Started HathDL processing from " + processFile)
          var gid = 0
          var files = 0
          var title:String = null
          var information = ""
          var galleryFiles:Array[GalleryFile]  = null
          var todir:File = null

          var parseState = 0
          val toParse = FileTools.getStringFileContentsUTF8(processFile).split("\n")

          toParse.foreach({(s:String) => 
          //for(s <- toParse {
            if(s.equals("FILELIST") && parseState == 0) {
                parseState = 1
                //continue
                return
            }

            if(s.equals("INFORMATION") && parseState == 1) {
                parseState = 2
                return
                //continue
            }

            if(parseState == 0) {
                if(s.isEmpty()) {
                    //continue
                    return
                }

                val split = s.split(" ", 2)

                if(split(0).equals("GID")) {
                    gid = split(1).toInt
                } else if(split(0).equals("FILES")) {
                    files = split(1).toInt
                    galleryFiles = new Array[GalleryFile](files)
                } else if(split(0).equals("TITLE")) {
                    title = split(1)
                      .replaceAll("(\\*|\\\"|\\\\|<|>|:\\|\\?)", "")
                      .replaceAll("\\s+", " ")
                      .replaceAll("(^\\s+|\\s+$)", "")
                    if(title.length() > 100) {
                        todir = new File(downloadeddir, title.substring(0, 97) + "... [" + gid + "]")
                    } else {
                        todir = new File(downloadeddir, title + " [" + gid + "]")
                    }
                    // just in case, check for directory traversal
                    if(! todir.getParentFile().equals(downloadeddir)) {
                        Out.warning("Downloader: Security Error - HHDL target download directory isn't where it's supposed to be. Aborted HHDL.")
                        gid = 0
                        break
                    }
                    Out.debug("Downloader: Created directory " + todir)
                    FileTools.checkAndCreateDir(todir)
                }
            } else if(parseState == 1) {
                if(s.isEmpty()) {
                    //continue
                    return
                }

                val split = s.split(" ", 3)
                val page = Integer.parseInt(split(0))
                val gf = GalleryFile.getGalleryFile(client, todir, split(1), gid, page, split(2))

                if(gf != null) {
                    if(page >= 1 && page <= files) {
                        galleryFiles(page - 1) = gf
                    } else {
                        Out.warning("File " + gf.getFileid() + " is outside allowed page range.")
                    }
                }
            } else {
                information = information.concat(s).concat(Settings.NEWLINE)
            }
          })

          if((gid > 0) && (files > 0) && (title != null) && (galleryFiles != null)) {
              pendingGalleries :+= new Gallery(client, processFile, todir, title, information, galleryFiles)
              processedHHDLFiles.add(processFile)
          }
        }
      }
    } catch{ case e: java.io.IOException => {
        Out.warning("Downloader: Encountered I/O error while processing HHDL files.")
        e.printStackTrace()
    }}





    if(! pendingGalleries.isEmpty) {
      var doDownload = true

      if(!Settings.isSkipFreeSpaceCheck()) {
        val diskFreeSpace = downloadeddir.getFreeSpace()

        if(diskFreeSpace < Math.max(Settings.getDiskMinRemainingBytes(), 104857600)) {
          Out.warning("Downloader: There is less than the minimum allowed space left on the storage device. The Hentai@Home Downloader is waiting for more disk space before it can continue.")
          sleepTime = 300000
          doDownload = false
        }
      }

      if(doDownload) {
        var galleryFiles:List[GalleryFile] = List()
        var completed:List[Gallery] = List()

        breakable {for(g <- pendingGalleries) {
          if(client.isShuttingDown()) {
            break
          }
          g.galleryPass(galleryFiles)
          if(g.getState() != Gallery.STATE_PENDING) {
            completed :+= g
          }
        }}

        //for(g <- completed) {
          //pendingGalleries.remove(g)
          //pendingGalleries = pendingGalleries diff List(g)
        //}
        pendingGalleries = pendingGalleries diff completed

        if( !galleryFiles.isEmpty && (lastTokenRequest < System.currentTimeMillis() - 60000) ) {
          // request up to 20 tokens a minute from the server

          val requestTokens = galleryFiles.map(_.getFileid())

          lastTokenRequest = System.currentTimeMillis()
          //Hashtable<String, String>
          val tokens = client.getServerHandler().getFileTokens(requestTokens)

          if(tokens == null) {
            sleepTime = 180000
          } else {

            for(gf <- galleryFiles) {
              /*tokens.get(gf.getFileid()) match {
                case Some(token) => gf.setNewToken(token)
              }*/
              gf.setNewToken(tokens.get(gf.getFileid()))
            }

          }
        }
      }
    }




  }
}
// */
