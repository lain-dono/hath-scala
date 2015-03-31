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

class ServerResponse(responseStatus: Int, responseText: Array[String], failCode: String) {
  //def getResponseStatus() = responseStatus
  def getResponseText() = responseText
  def getFailCode() = failCode

  def isOk()   = responseStatus == ServerResponse.RESPONSE_STATUS_OK
  def isNull() = responseStatus == ServerResponse.RESPONSE_STATUS_NULL
  def isFail() = responseStatus == ServerResponse.RESPONSE_STATUS_FAIL

  def this(responseStatus:Int, responseText: Array[String]) = this(responseStatus, responseText, null)
  def this(responseStatus:Int, failCode: String) = this(responseStatus, null, failCode)

  override def toString():String = {
    var sb = ""
    if(responseText != null) {
      for(i <- 0 to responseText.length-1) {
        sb += responseText(i) + ","
      }
    }
    "ServerResponse {responseStatus=" + responseStatus + ", responseText=" + sb + ", failCode=" + failCode + "}"
  }
}

object ServerResponse {
  val RESPONSE_STATUS_NULL = 0
  val RESPONSE_STATUS_OK = 1
  val RESPONSE_STATUS_FAIL = -1

  def getServerResponse(act: String, retryhandler: ServerHandler): ServerResponse = {
    val serverConnectionURL = ServerHandler.getServerConnectionURL(act)
    getServerResponse(serverConnectionURL, retryhandler, act)
  }

  def getServerResponse(serverConnectionURL: URL, retryhandler: ServerHandler): ServerResponse =
    getServerResponse(serverConnectionURL, retryhandler, null)

  private def getServerResponse(serverConnectionURL: URL, retryhandler: ServerHandler, retryact: String): ServerResponse = {
      val dler = new FileDownloader(serverConnectionURL, 3600000)
      val serverResponse = dler.getTextContent()

      if(serverResponse == null) {
          return new ServerResponse(RESPONSE_STATUS_NULL, "NO_RESPONSE")
      } else if(serverResponse.length() < 1) {
          return new ServerResponse(RESPONSE_STATUS_NULL, "NO_RESPONSE")
      }

      val split = serverResponse.split("\n")

      Out.debug("Received response: " + serverResponse)

      if(split.length < 1) {
          new ServerResponse(RESPONSE_STATUS_NULL, "NO_RESPONSE")
      }
      else if(split(0).startsWith("Log Code") || split(0).startsWith("Database Error")) {
          new ServerResponse(RESPONSE_STATUS_NULL, "SERVER_ERROR")
      }
      else if(split(0).startsWith("TEMPORARILY_UNAVAILABLE")) {
          new ServerResponse(RESPONSE_STATUS_NULL, "TEMPORARILY_UNAVAILABLE")
      }
      else if(split(0).equals("OK")) {
          Stats.serverContact()
          new ServerResponse(RESPONSE_STATUS_OK, java.util.Arrays.copyOfRange(split, 1, split.length))
      }
      else if(split(0).equals("KEY_EXPIRED") && retryhandler != null && retryact != null) {
          Out.warning("Server reported expired key attempting to refresh time from server and retrying")
          retryhandler.refreshServerStat()
          getServerResponse(ServerHandler.getServerConnectionURL(retryact), null)
      }
      else {
          new ServerResponse(RESPONSE_STATUS_FAIL, split(0))
      }
  }
}
