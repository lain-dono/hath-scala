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

//import scala.collection.mutable.MutableList

import java.util.Date
import java.util.TimeZone
import java.text.SimpleDateFormat
//import java.util.List
import java.util.ArrayList
import java.io.File
import java.io.PrintStream
import java.io.OutputStream
import java.io.FileWriter

object Out {
  val DEBUG = 1
  val INFO = 2
  val WARNING = 4
  val ERROR = 8

  val LOGOUT = DEBUG | INFO | WARNING | ERROR
  val LOGERR = WARNING | ERROR
  val OUTPUT = INFO | WARNING | ERROR
  val VERBOSE = ERROR

  private var suppressedOutput = 0
  private var overridden = false
  private var writeLogs = false
  private var def_out: PrintStream = null
  private var def_err: PrintStream = null
  private var or_out: OutPrintStream = null
  private var or_err: OutPrintStream = null
  private var logout: FileWriter = null
  private var logerr: FileWriter = null
  private var logout_count = 0
  private var logerr_count = 0

  private var sdf: SimpleDateFormat = null

  private var outListeners: Set[OutListener] = null

  try {
    Settings.initializeDataDir()
  } catch {
    case ioe: java.io.IOException => {
      System.err.println("Could not create data directory. Please check file access permissions and free disk space.")
      System.exit(-1)
    }
  }
  overrideDefaultOutput()

  def overrideDefaultOutput() {
    if(overridden) {
      return
    }

    writeLogs = true
    overridden = true
    outListeners = Set[OutListener]()

    suppressedOutput = 0

    sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'") // ISO 8601
    sdf.setTimeZone(TimeZone.getTimeZone("UTC"))
    def_out = System.out
    def_err = System.err

    or_out = new OutPrintStream(def_out, "out", INFO)
    or_err = new OutPrintStream(def_err, "ERR", ERROR)
    System.setOut(or_out)
    System.setErr(or_err)

    logout = startLogger(Settings.getOutputLogPath())
    logerr = startLogger(Settings.getErrorLogPath())
  }

  def addOutListener(listener: OutListener) {
    outListeners.synchronized {
      outListeners += listener
    }
  }

  def removeOutListener(listener: OutListener) {
    outListeners.synchronized {
      outListeners -= listener
    }
  }

  def disableLogging() {
    if(writeLogs) {
      info("Logging ended.")
      writeLogs = false
      flushLogs()
    }
  }

  def flushLogs() {
    try {
      logout.flush()
    } catch {
      case e: Exception =>
    }
  }

  private def startLogger(logfile: String): FileWriter = {
    // delete existing old logs
    List(0, 1, 2, 3).foreach(
      (i:Int) => (new File(logfile + "." + i)).delete()
    )

    (new File(logfile + ".old")).delete()
    (new File(logfile)).renameTo(new File(logfile + ".old"))

    var logger: FileWriter = null

    if(logfile != null && logfile.length() > 0) {
      try {
        logger = new FileWriter(logfile, true)
      } catch {
        case e: java.io.IOException => {
          e.printStackTrace()
          System.err.println("Failed to open log file " + logfile)
        }
      }
    }

    if(logger != null) {
      Out.info("Started logging to " + logfile)
      log("", logger)
      log("*********************************************************", logger)
      log(sdf.format(new Date()) + " Logging started", logger)
    }

    return logger
  }

  private def stopLogger(logger: FileWriter): Boolean = {
    try {
      logger.close()
    } catch {
      case e: Exception => {
        e.printStackTrace(def_err)
        def_err.println("Unable to close file writer handle: Cannot rotate log.")
        return false
      }
    }
    return true
  }

  def debug(x: String)    = or_out.println(x, "debug", DEBUG)
  def info(x: String)     = or_out.println(x, "info", INFO)
  def warning(x: String)  = or_out.println(x, "WARN", WARNING)
  def error(x: String)    = or_out.println(x, "ERROR", ERROR)

  private def log(data: String, severity: Int) {
    if( ((severity & LOGOUT) > 0) && writeLogs ) {
      log(data, logout, false)
      logout_count += 1
      if(logout_count > 100000) {
        logout_count = 0
        def_out.println("Rotating output logfile...")
        if(stopLogger(logout)) {
          logout = startLogger(Settings.getOutputLogPath())
          def_out.println("Output logfile rotated.")
        }
      }
    }

    if ((severity & LOGERR) > 0) {
      log(data, logerr, true)
      logerr_count += 1
      if(logerr_count > 100000) {
        logerr_count = 0
        def_out.println("Rotating error logfile...")
        if(stopLogger(logerr)) {
          logerr = startLogger(Settings.getErrorLogPath())
          def_out.println("Error logfile rotated.")
        }
      }
    }
  }

  private def log(data: String, writer: FileWriter) {
    log(data, writer, false)
  }

  private def log(data: String, writer: FileWriter, flush: Boolean) {
    if(writer != null) {
      writer.synchronized {
        try {
          writer.write(data + "\n")
          if(flush) {
            writer.flush()
          }
        } catch {
          case ioe: java.io.IOException =>{
            // IMPORTANT: writes to the default System.err to prevent loops
            ioe.printStackTrace(def_err)
          }
        }
      }
    }
  }

  def verbose(severity: Int): String = {
    if ((severity & VERBOSE) == 0) "" else {
        //java.lang.StackTraceElement[]
        val ste = java.lang.Thread.currentThread().getStackTrace()

        val last = ste.find((el:StackTraceElement) => el.getClassName match{
          case "org.hath.base.Out" => false
          case "org.hath.base.Out$OutPrintStream" => false
          case "java.lang.Thread" => false
          case _ => true
        })

        last match {
          case Some(el) =>
            if (el.getClassName() == "java.lang.Throwable") "" else "{" + el + "} "
          case None => "{Unknown Source}"
        }
    }
  }

  private class OutPrintStream(ps:PrintStream, name:String, severity:Int) extends PrintStream(ps) {
    override def println(x:String) = println(x, name, severity)

    override def println(x:Boolean) = println(String.valueOf(x))
    override def println(x: Array[Char]) = println(String.valueOf(x))
    override def println(x: Double) = println(String.valueOf(x))
    override def println(x: Float) = println(String.valueOf(x))
    override def println(x: Char) = println(String.valueOf(x))
    override def println(x: Int) = println(String.valueOf(x))
    override def println(x: Long) = println(String.valueOf(x))
    override def println(x: Any) = println(String.valueOf(x))

    def println(x:String, name:String):Unit = println(x, name, severity)

    def println(x:String, name:String, severity:Int):Unit = {
      if(x == null) {
          return
      }

      val output = (severity & Out.OUTPUT & ~Out.suppressedOutput) > 0
      val log = (severity & (Out.LOGOUT | Out.LOGERR)) > 0
      if(output || log) {
        outListeners.synchronized {
          val v = Out.verbose(severity)
          x.split("\n").foreach((s: String) => {
            val data = sdf.format(new Date()) + " [" + name + "] " + v + s
            if(output) {
              ps.println(data)
              outListeners.foreach(_.outputWritten(data))
            }
            if(log) {
              Out.log(data, severity)
            }
          })
        }
      }
    }
  }
}
