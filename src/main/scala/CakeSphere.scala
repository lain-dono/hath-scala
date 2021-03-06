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

import java.lang.Thread

class CakeSphere(handler: ServerHandler, client: HentaiAtHomeClient) extends Runnable {
  private val myThread = new Thread(this)

  def stillAlive() {
    // Cake and grief counseling will be available at the conclusion of the test.
    myThread.start()
  }

  def run() {
    val sr = ServerResponse.getServerResponse(ServerHandler.ACT_STILL_ALIVE, handler)
    if(sr.isOk) {
      Out.debug("Successfully performed a stillAlive test for the server.")
    } else if(sr.isNull) {
      Out.warning("Failed to connect to the server for the stillAlive test. This is probably a temporary connection problem.")
    } else {
      Out.warning("Failed stillAlive test: (" + sr.getFailCode() + ") - will retry later")
    }
  }
}
