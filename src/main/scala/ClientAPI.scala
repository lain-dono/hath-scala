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

class ClientAPIResult(command:Int, resultText:String) {
  def isTextOk() = resultText == "OK"
  def getResultText() = resultText
  override def toString() = "{ClientAPIResult: command=" + command + ",  resultText=" + resultText + "}"
}

// note: this class can be invoked by local extentions to play with stuff
class ClientAPI(client: HentaiAtHomeClient) {
  // available hooks for controlling the client
  def clientSuspend(suspendTime:Int) = {
    val txt = if(client.suspendMasterThread(suspendTime)) "OK" else "FAIL"
    new ClientAPIResult(ClientAPI.API_COMMAND_CLIENT_SUSPEND, txt)
  }

  def clientResume() = {
    val txt = if(client.resumeMasterThread()) "OK" else "FAIL"
    new ClientAPIResult(ClientAPI.API_COMMAND_CLIENT_RESUME, txt)
  }

  def refreshSettings() = {
    val txt = if(client.getServerHandler().refreshServerSettings()) "OK" else "FAIL"
    new ClientAPIResult(ClientAPI.API_COMMAND_REFRESH_SETTINGS, txt)
  }
}

object ClientAPI {
  def API_COMMAND_CLIENT_START()     = 1
  def API_COMMAND_CLIENT_SUSPEND()   = 2
  def API_COMMAND_CLIENT_RESUME()    = 5
  def API_COMMAND_MODIFY_SETTING()   = 3
  def API_COMMAND_REFRESH_SETTINGS() = 4
}
