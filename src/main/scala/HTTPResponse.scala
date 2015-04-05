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

//import java.util.Hashtable
//import java.util.List
//import java.util.ArrayList
import java.util.regex.Pattern

class HTTPResponse(session:HTTPSession) {
  private val absoluteUriPattern = Pattern.compile("^http://[^/]+/", Pattern.CASE_INSENSITIVE)


  private var requestHeadOnly = false
  private var validRequest = false
  private var servercmd = false
  private var responseStatusCode = 500 // if nothing alters this, there's a bug somewhere

  private var hpc:HTTPResponseProcessor = null


  private def processRemoteAPICommand(command:String, additional:String):HTTPResponseProcessor = {
    val addTable = MiscTools.parseAdditional(additional)
    val client = session.getHTTPServer().getHentaiAtHomeClient()

    try {
      command.toLowerCase match {
        case "still_alive" =>
          return new HTTPResponseProcessorText("I feel FANTASTIC and I'm still alive")
        case "cache_list" =>
          return new HTTPResponseProcessorCachelist(client.getCacheHandler())
        case "cache_files" =>
          return new HTTPResponseProcessorText(client.getServerHandler().downloadFilesFromServer(addTable))
        case "proxy_test" => {
          val ipaddr = addTable.get("ipaddr")
          val port = addTable.get("port").toInt
          val fileid = addTable.get("fileid")
          val keystamp = addTable.get("keystamp")
          return new HTTPResponseProcessorText(client.getServerHandler().doProxyTest(ipaddr, port, fileid, keystamp))
        }
        case "threaded_proxy_test" => {
          val ipaddr = addTable.get("ipaddr")
          val port = addTable.get("port").toInt
          val testsize = addTable.get("testsize").toInt
          val testcount = addTable.get("testcount").toInt
          val testtime = addTable.get("testtime").toInt
          val testkey = addTable.get("testkey")
          return new HTTPResponseProcessorText(client.getServerHandler().doThreadedProxyTest(ipaddr, port, testsize, testcount, testtime, testkey))
        }
        case "speed_test" => {
          val testsize = addTable.get("testsize")
          return new HTTPResponseProcessorSpeedtest(if(testsize != null) testsize.toInt else 1000000)
        }
        case "refresh_settings" =>
          return new HTTPResponseProcessorText(client.getServerHandler().refreshServerSettings()+"")
      }
    } catch{case e:Exception => {
        e.printStackTrace()
        Out.warning(session + " Failed to process command")
    }}

    return new HTTPResponseProcessorText("INVALID_COMMAND")
  }

  def parseRequest(request:String, localNetworkAccess:Boolean) {
    if(request == null) {
      responseStatusCode = 400
      return
    }

    val requestParts = request.trim().split(" ", 3)

    if(requestParts.length != 3) {
      Out.warning(session + " Invalid HTTP request form.")
    } else if( !(requestParts(0).equalsIgnoreCase("GET") || requestParts(0).equalsIgnoreCase("HEAD")) || !requestParts(2).startsWith("HTTP/") ) {
      Out.warning(session + " HTTP request is not GET or HEAD.")
      responseStatusCode = 405
      return
    } else {
      validRequest = true
      requestHeadOnly = requestParts(0).equalsIgnoreCase("HEAD")

      // The request URI may be an absolute path or an absolute URI for GET/HEAD requests (see section 5.1.2 of RFC2616)
      requestParts(1) = absoluteUriPattern.matcher(requestParts(1)).replaceFirst("/")

      val urlparts = requestParts(1).replace("%3d", "=").split("/")

      if( (urlparts.length < 2) || !urlparts(0).equals("")) {
          Out.warning(session + " The requested URL is invalid or not supported.")
      } else {
          if(urlparts(1).equals("h")) {
            h(urlparts, localNetworkAccess)
            return
          }
          else if(urlparts(1).equals("servercmd")) {
            servercmd(urlparts)
          }
          else if(urlparts(1).equals("p")) {
            p(urlparts)
            return
          }
          else if(urlparts(1).equals("t")) {
            // sends a randomly generated file of a given length for speed testing purposes
            if(urlparts.length < 5) {
              responseStatusCode = 400
            } else {
              val testsize = urlparts(2).toInt
              val testtime = urlparts(3).toInt
              val testkey = urlparts(4)

              Out.debug("Sending threaded proxy test with testsize=" + testsize + " testtime=" + testtime + " testkey=" + testkey)
              if(Math.abs(testtime - Settings.getServerTime()) > Settings.MAX_KEY_TIME_DRIFT) {
                  Out.warning(session + " Got a speedtest request with expired key")
                  responseStatusCode = 403
              } else if(!MiscTools.getSHAString("hentai@home-speedtest-" + testsize + "-" + testtime + "-" + Settings.getClientID() + "-" + Settings.getClientKey()).equals(testkey)) {
                  Out.warning(session + " Got a speedtest request with invalid key")
                  responseStatusCode = 403
              } else {
                responseStatusCode = 200
                hpc = new HTTPResponseProcessorSpeedtest(testsize)
              }
            }
            return
          }
          else if(urlparts.length == 2) {
              if(urlparts(1).equals("favicon.ico")) {
                // Redirect to the main website icon (which should already be in the browser cache).
                hpc = new HTTPResponseProcessorText("")
                hpc.addHeaderField("Location", "http://g.e-hentai.org/favicon.ico")
                responseStatusCode = 301 // Moved Permanently
                return
              }
              else if(urlparts(1).equals("robots.txt")) {
                // Bots are not welcome.
                hpc = new HTTPResponseProcessorText("User-agent: *\nDisallow: /", "text/plain")
                responseStatusCode = 200 // Found
                return
              }
              else {
                Out.warning(session + " Invalid request type '" + urlparts(1) + "'.")
              }
          }
          else {
              Out.warning(session + " Invalid request type '" + urlparts(1) + "'.")
          }
      }

      responseStatusCode = 404
      return
    }

    Out.warning(session + " Invalid HTTP request.")
    responseStatusCode = 400
  }

  private def h(urlparts: Array[String], localNetworkAccess:Boolean) {
    if(urlparts.length < 4) {
      responseStatusCode = 400
      return
    }

    // new url type for H@H.. we don't really do anything new, but having the filename at the end will make browsers using this as filename per default.
    // we also put in an extension that allows us to add additional arguments to the request url without messing with old clients.

    val hvfile = urlparts(2)
    val additional = MiscTools.parseAdditional(urlparts(3))
    // urlparts[4] will contain the filename, but we don't actively use this

    val requestedHVFile = session.getHTTPServer().getHentaiAtHomeClient().getCacheHandler().getHVFile(hvfile, !localNetworkAccess)

    var keystampRejected = true
    val keystampParts = additional.get("keystamp").split("-")

    if(keystampParts.length == 2) {
      try {
          val keystampTime = keystampParts(0).toLong

          if(Math.abs(Settings.getServerTime() - keystampTime) < 900) {
              if(keystampParts(1).equalsIgnoreCase( MiscTools.getSHAString(keystampTime + "-" + hvfile + "-" + Settings.getClientKey() + "-hotlinkthis").substring(0, 10) )) {
                  keystampRejected = false
              }
          }
      } catch{case e:Exception =>}
    }

    if(keystampRejected) {
      responseStatusCode = 403
    } else if(requestedHVFile == null) {
      Out.warning(session + " The requested file was invalid or not found in cache.")
      responseStatusCode = 404
    } else {
      val fileid = requestedHVFile.getFileid()

      if(requestedHVFile.getLocalFileRef().exists()) {
        hpc = new HTTPResponseProcessorFile(requestedHVFile)
      } else if(Settings.isStaticRange(fileid)) {
        // non-existent file in a static range. do an on-demand request of the file from the image servers
        val tokens = session.getHTTPServer().getHentaiAtHomeClient().getServerHandler().getFileTokens(List(fileid))

        if(tokens.containsKey(fileid)) {
          hpc = new HTTPResponseProcessorProxy(session, fileid, tokens.get(fileid), 1, 1, "ondemand")
        } else {
          responseStatusCode = 404
        }
      } else {
        // file does not exist, and is not in one of the client's static ranges
        responseStatusCode = 404
      }
    }
  }

  private def servercmd(urlparts: Array[String]) {
    // form: /servercmd/$command/$additional/$time/$key

    if(!Settings.isValidRPCServer(session.getSocketInetAddress())) {
      Out.warning(session + " Got a servercmd from an unauthorized IP address: Denied")
      responseStatusCode = 403
    } else if(urlparts.length < 6) {
      Out.warning(session + " Got a malformed servercmd: Denied")
      responseStatusCode = 403
    } else {
      val command = urlparts(2)
      val additional = urlparts(3)
      val commandTime = urlparts(4).toInt
      val key = urlparts(5)

      val correctedTime = Settings.getServerTime()

      if((Math.abs(commandTime - correctedTime) < Settings.MAX_KEY_TIME_DRIFT) && MiscTools.getSHAString("hentai@home-servercmd-" + command + "-" + additional + "-" + Settings.getClientID() + "-" + commandTime + "-" + Settings.getClientKey()).equals(key)) {
        responseStatusCode = 200
        servercmd = true
        hpc = processRemoteAPICommand(command, additional)
      } else {
        Out.warning(session + " Got a servercmd with expired or incorrect key: Denied")
        responseStatusCode = 403
      }
    }
  }

  private def p(urlparts: Array[String]) {
    // new proxy request type, used implicitly when the password field is set
    // form: /p/fileid=asdftoken=asdfgid=123page=321passkey=asdf/filename

    // we allow access depending on the proxy mode retrieved from the server when the client is first started.
    // 0 = disabled
    // 1 = local networks open
    // 2 = external networks open
    // 3 = local network protected
    // 4 = external network protected

    val proxymode = Settings.getRequestProxyMode()
    val enableProxy = proxymode > 0
    val requirePasskey = proxymode == 3 || proxymode == 4
    val requireLocalNetwork = proxymode == 1 || proxymode == 3

    if( !enableProxy || (requireLocalNetwork && !session.isLocalNetworkAccess()) ) {
      Out.warning(session + " Proxy request denied for remote client.")
    } else {
      val parsedRequest = MiscTools.parseAdditional(urlparts(2))

      val fileid   = parsedRequest.get("fileid")
      val token    = parsedRequest.get("token")
      val szGid    = parsedRequest.get("gid")
      val szPage   = parsedRequest.get("page")
      val passkey  = parsedRequest.get("passkey")
      val filename = urlparts(3)

      var acceptPasskey = false

      if(!requirePasskey) {
        acceptPasskey = true
      } else if(passkey != null) {
        // The client's passkey is generated by passing the client key through a SHA-1 function together with an additional string. This passkey is then entered under My Settings.
        // The request passkey is generated by passing the client passkey through an additional SHA-1 operation, which also includes the fileid of the requested file. This gives an unique passkey for each request.

        val expectedPasskey = MiscTools.getSHAString(
            fileid + "I think we can put our differences behind us." + MiscTools.getSHAString(Settings.getClientKey() + "For science.").substring(0, 10) + "You monster."
        ).substring(0, 10)

        if(expectedPasskey.equals(passkey)) {
          acceptPasskey = true
        }
      }

      if(!acceptPasskey) {
        Out.warning(session + " Invalid passkey")
      } else if( !(HVFile.isValidHVFileid(fileid) && token.matches("^\\d+-[a-z0-9]{40}$") && szGid.matches("^\\d+$") && szPage.matches("^\\d+$") && filename.matches("^(([a-zA-Z0-9])|(\\.)|(_))*$")) ) {
        Out.warning(session + " Failed argument validation")
      } else {
        try {
          val gid = szGid.toInt
          val page = szPage.toInt

          if(gid > 0 && page > 0) {
            val requestedHVFile = session.getHTTPServer().getHentaiAtHomeClient().getCacheHandler().getHVFile(fileid, true)

            if( (requestedHVFile != null) && (requestedHVFile.getLocalFileRef().exists()) ) {
              hpc = new HTTPResponseProcessorFile(requestedHVFile)
            } else {
              hpc = new HTTPResponseProcessorProxy(session, fileid, token, gid, page, filename)
            }
          } else {
            Out.warning(session + " gid and/or page are <= 0")
            // TODO 404
          }
        } catch{ case e:Exception =>
          Out.warning(session + " gid and/or page are not valid integers")
        }
      }
    }
  }

  def getHTTPResponseProcessor():HTTPResponseProcessor = {
    if(hpc == null) {
      //Out.info(session + " The remote host made an invalid request that could not be serviced.")
      hpc = new HTTPResponseProcessorText("An error has occurred. (" + responseStatusCode + ")")
      if(responseStatusCode == 405) {
        hpc.addHeaderField("Allow", "GET,HEAD")
      }
    }
    else if(hpc.isInstanceOf[HTTPResponseProcessorFile]) {
      responseStatusCode = hpc.initialize()
    }
    else if(hpc.isInstanceOf[HTTPResponseProcessorProxy]) {
      responseStatusCode = hpc.initialize()
    }
    else if(hpc.isInstanceOf[HTTPResponseProcessorText]) {
      // do nothing
    }
    else if(hpc.isInstanceOf[HTTPResponseProcessorSpeedtest]) {
      Stats.setProgramStatus("Running speed tests...")
    }
    else if(hpc.isInstanceOf[HTTPResponseProcessorCachelist]) {
      Stats.setProgramStatus("Building and sending cache list to server...")
      hpc.initialize()
    }

    return hpc
  }


  // accessors

  def getResponseStatusCode() = responseStatusCode
  def isValidRequest() =validRequest
  def isRequestHeadOnly() = requestHeadOnly
  def isServercmd() = servercmd
}
