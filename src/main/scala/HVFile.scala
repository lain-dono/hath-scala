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

// note: this class does not necessarily represent an actual file even though it is occasionally used as such (getLocalFileRef()) - it is an abstract representation of files in the HentaiVerse System

import java.io.File

class HVFile(val hash:String, val size:Int, val xres:Int, val yres:Int, val _type:String) {
  def getLocalFileRef() = new File(CacheHandler.getCacheDir(), hash.substring(0, 2) + "/" + getFileid())

  def getFileid() = hash + "-" + size + "-" + xres + "-" + yres + "-" + _type

  def getHash() = hash
  def getSize() = size
  def getType() = _type

  def getMimeType() = _type match {
    case "jpg" => Settings.CONTENT_TYPE_JPG
    case "png" => Settings.CONTENT_TYPE_PNG
    case "gif" => Settings.CONTENT_TYPE_GIF
    case _     => Settings.CONTENT_TYPE_OCTET
  }

  def localFileMatches(file: File) {
    // note: we only check the sha-1 hash and filesize here, to save resources and avoid dealing with the crummy image handlers
    try {
      return file.length() == size && hash.startsWith(MiscTools.getSHAString(file))
    } catch {
      case e: java.io.IOException => Out.warning("Failed reading file " + file + " to determine hash.")
    }
    return false
  }

  override def toString() = getFileid()
}

// static stuff
object HVFile {
  def isValidHVFileid(fileid: String): Boolean
    = java.util.regex.Pattern.matches("^[a-f0-9]{40}-[0-9]{1,8}-[0-9]{1,5}-[0-9]{1,5}-((jpg)|(png)|(gif))$", fileid)

  def getHVFileFromFile(file: File, verify: Boolean): HVFile = {
    if(file.exists()) {
      val fileid = file.getName()

      try {
        if(verify) {
          if(!fileid.startsWith(MiscTools.getSHAString(file))) {
            return null
          }
        }

        return getHVFileFromFileid(fileid)
      } catch {
        case e: java.io.IOException => {
          e.printStackTrace()
          Out.warning("Warning: Encountered IO error computing the hash value of " + file)
        }
      }
    }

    return null
  }

  def getHVFileFromFileid(fileid: String): HVFile = {
    if(isValidHVFileid(fileid)) {
      try {
        val fileidParts = fileid.split("-")
        val hash = fileidParts(0)
        val size = Integer.parseInt(fileidParts(1))
        val xres = Integer.parseInt(fileidParts(2))
        val yres = Integer.parseInt(fileidParts(3))
        val _type = fileidParts(4)
        return new HVFile(hash, size, xres, yres, _type)
      } catch {
        case e: Exception =>
          Out.warning("Failed to parse fileid \"" + fileid + "\" : " + e)
      }
    } else {
      Out.warning("Invalid fileid \"" + fileid + "\"")
    }

    return null
  }
}
