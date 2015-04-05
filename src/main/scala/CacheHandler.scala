/*

Copyright 2008-2014 E-Hentai.org
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
//import java.util.ArrayList
//import java.util.Collections
//import java.util.LinkedList
//import java.util.List
import java.sql.{Connection, PreparedStatement, DriverManager}

import scala.collection.mutable.Map
import scala.collection.mutable.MutableList

class CacheHandler(client:HentaiAtHomeClient) {
  //private var client:HentaiAtHomeClient = null
  private var sqlite:Connection = null

  private var cacheCount=0
  private var startupCachedFileStrlen = 0
  private var cacheSize:Long = 0
  private var quickStart = false

  protected var cacheIndexClearActive:PreparedStatement = null
  protected var cacheIndexCountStats:PreparedStatement = null

  protected var queryCachelistSegment:PreparedStatement = null
  protected var queryCachedFileLasthit:PreparedStatement = null
  protected var queryCachedFileSortOnLasthit:PreparedStatement = null

  protected var insertCachedFile:PreparedStatement = null
  protected var updateCachedFileLasthit:PreparedStatement = null
  protected var updateCachedFileActive:PreparedStatement = null

  protected var deleteCachedFile:PreparedStatement = null
  protected var deleteCachedFileInactive:PreparedStatement = null

  protected var getStringVar:PreparedStatement = null
  protected var setStringVar:PreparedStatement = null

  protected var recentlyAccessed:List[CachedFile] = List() // len 100
  protected var pendingRegister:List[HVFile] = List() // len 50
  protected var recentlyAccessedFlush:Long = 0

  private var memoryWrittenTable:Array[Short] = null
  private var memoryClearPointer = 0

  private val CLEAN_SHUTDOWN_KEY = "clean_shutdown"
  private val CLEAN_SHUTDOWN_VALUE = "clean_r81"
  private val MEMORY_TABLE_ELEMENTS = 1048576

  //@throws(classOf[java.io.IOException])
  //def this(client:HentaiAtHomeClient) = {
    //this.client = client

  //CacheHandler(HentaiAtHomeClient client) throws java.io.IOException {
      //this.client = client
      //this.recentlyAccessed = new ArrayList<CachedFile>(100)
      //this.pendingRegister = new ArrayList<HVFile>(50)

      CacheHandler.tmpdir = FileTools.checkAndCreateDir(new File("tmp"))
      CacheHandler.cachedir = FileTools.checkAndCreateDir(new File("cache"))

      if(!Settings.isUseLessMemory()) {
          // the memoryWrittenTable can hold 16^5 = 1048576 shorts consisting of 16 bits each.
          // addressing is done by looking up the first five nibbles (=20 bits) of a hash, then using the sixth nibble to determine which bit in the short to read/set.
          // while collisions may occur, they should be fairly rare, and should not cause any major issues with files not having their timestamp updated.
          // (and even if it does, the impact of this will be negligible, as it will only cause the LRU mechanism to be slightly less efficient.)
          memoryWrittenTable = new Array[Short](MEMORY_TABLE_ELEMENTS)
          memoryClearPointer = 0
      }

      Out.info("CacheHandler: Initializing database engine...")

      try {
        if( !initializeDatabase("data/hath.db") ) {
          Out.info("")
          Out.info("**************************************************************************************************************")
          Out.info("The database could not be loaded. Please check file permissions and file system integrity.")
          Out.info("If everything appears to be working, please do the following:")
          Out.info("1. Locate the directory " + Settings.getDataDir().getAbsolutePath())
          Out.info("2. Delete the file hath.db")
          Out.info("3. Restart the client.")
          Out.info("The system should now rebuild the database.")
          Out.info("***************************************************************************************************************")
          Out.info("")

          HentaiAtHomeClient.dieWithError("Failed to load the database.")
        }

        if(quickStart) {
          Out.info("Last shutdown was clean - using fast startup procedure.")
        } else {
          Out.info("Last shutdown was dirty - the cache index must be verified.")
        }

        Out.info("CacheHandler: Database initialized")
      }
      catch{case e:Exception => {
          Out.error("CacheHandler: Failed to initialize SQLite database engine")
          HentaiAtHomeClient.dieWithError(e)
      }}
  //}

  private def initializeDatabase(db:String):Boolean = {
      try {
        Out.info("CacheHandler: Loading database from " + db)

        Class.forName("org.sqlite.JDBC")
        sqlite = DriverManager.getConnection("jdbc:sqlite:" + db)
        val dma = sqlite.getMetaData()
        Out.info("CacheHandler: Using " + dma.getDatabaseProductName() + " " + dma.getDatabaseProductVersion() + " over " + dma.getDriverName() + " " + dma.getJDBCMajorVersion() + "." + dma.getJDBCMinorVersion() + " running in " + dma.getDriverVersion() + " mode")

        Out.info("CacheHandler: Initializing database tables...")
        val stmt = sqlite.createStatement()
        stmt.executeUpdate("CREATE TABLE IF NOT EXISTS CacheList (fileid VARCHAR(65)  NOT NULL, lasthit INT UNSIGNED NOT NULL, filesize INT UNSIGNED NOT NULL, active BOOLEAN NOT NULL, PRIMARY KEY(fileid))")
        stmt.executeUpdate("CREATE INDEX IF NOT EXISTS Lasthit ON CacheList (lasthit DESC)")
        stmt.executeUpdate("CREATE TABLE IF NOT EXISTS StringVars ( k VARCHAR(255) NOT NULL, v VARCHAR(255) NOT NULL, PRIMARY KEY(k) )")

        cacheIndexClearActive = sqlite.prepareStatement("UPDATE CacheList SET active=0")
        cacheIndexCountStats = sqlite.prepareStatement("SELECT COUNT(*), SUM(filesize) FROM CacheList")
        queryCachelistSegment = sqlite.prepareStatement("SELECT fileid FROM CacheList WHERE fileid BETWEEN ? AND ?")
        queryCachedFileLasthit = sqlite.prepareStatement("SELECT lasthit FROM CacheList WHERE fileid=?")
        queryCachedFileSortOnLasthit = sqlite.prepareStatement("SELECT fileid, lasthit, filesize FROM CacheList ORDER BY lasthit LIMIT ?, ?")
        insertCachedFile = sqlite.prepareStatement("INSERT OR REPLACE INTO CacheList (fileid, lasthit, filesize, active) VALUES (?, ?, ?, 1)")
        updateCachedFileActive = sqlite.prepareStatement("UPDATE CacheList SET active=1 WHERE fileid=?")
        updateCachedFileLasthit = sqlite.prepareStatement("UPDATE CacheList SET lasthit=? WHERE fileid=?")
        deleteCachedFile = sqlite.prepareStatement("DELETE FROM CacheList WHERE fileid=?")
        deleteCachedFileInactive = sqlite.prepareStatement("DELETE FROM CacheList WHERE active=0")
        setStringVar = sqlite.prepareStatement("INSERT OR REPLACE INTO StringVars (k, v) VALUES (?, ?)")
        getStringVar = sqlite.prepareStatement("SELECT v FROM StringVars WHERE k=?")

        try {
          // convert and clear pre-r81 tablespace if present. this will trip an exception if the table doesn't exist and skip the rest of the conversion block
          stmt.executeUpdate("UPDATE CacheIndex SET active=0")

          Out.info("Updating database schema to r81...")
          var hashtable:Map[String, Long] = Map()
          val rs = stmt.executeQuery("SELECT fileid, lasthit FROM CacheIndex")
          while(rs.next()) {
            hashtable.put(rs.getString(1), rs.getLong(2))
          }
          rs.close()

          sqlite.setAutoCommit(false)

          hashtable.foreach { case(fileid, value) =>
            //for(java.util.Enumeration<String> keys = hashtable.keys(); keys.hasMoreElements(); ) {
            //String fileid = keys.nextElement()

            insertCachedFile.setString(1, fileid)
            //insertCachedFile.setLong(2, hashtable.get(fileid).longValue())
            insertCachedFile.setLong(2, value)
            insertCachedFile.setInt(3, HVFile.getHVFileFromFileid(fileid).getSize())
            insertCachedFile.executeUpdate()
          }

          sqlite.setAutoCommit(true)

          stmt.executeUpdate("DROP TABLE CacheIndex")

          Out.info("Database updates complete")
        }
        catch{case e:Exception =>}

        resetFutureLasthits()

        Out.info("CacheHandler: Optimizing database...")
        stmt.executeUpdate("VACUUM")

        // are we dirty?
        if(!Settings.isForceDirty()) {
            getStringVar.setString(1, CLEAN_SHUTDOWN_KEY)
            val rs = getStringVar.executeQuery()
            if(rs.next()) {
              quickStart = rs.getString(1).equals(CLEAN_SHUTDOWN_VALUE)
            }
            rs.close()
        }

        setStringVar.setString(1, CLEAN_SHUTDOWN_KEY)
        setStringVar.setString(2, System.currentTimeMillis() + "")
        setStringVar.executeUpdate()

        return true
      } catch{ case e:Exception => {
        Out.error("CacheHandler: Encountered error reading database.")
        e.printStackTrace()
        terminateDatabase()
      }}

      false
  }

  private def resetFutureLasthits() /*throws SQLException*/ {
    val nowtime = Math.floor(System.currentTimeMillis() / 1000).toLong

    sqlite.setAutoCommit(false)

    Out.info("CacheHandler: Checking future lasthits on non-static files...")

    val getFutureLasthits = sqlite.prepareStatement("SELECT fileid FROM CacheList WHERE lasthit>?")
    getFutureLasthits.setLong(1, nowtime + 2592000)
    val rs = getFutureLasthits.executeQuery()

    //List<String> removelist:List[String] = Collections.checkedList(new ArrayList<String>(), String.class)
    var removelist:List[String] = List()

    while( rs.next() ) {
      val fileid = rs.getString(1)
      if( !Settings.isStaticRange(fileid) ) {
        removelist :+= fileid
      }
    }

    rs.close()

    //PreparedStatement resetLasthit = sqlite.prepareStatement("UPDATE CacheList SET lasthit=? WHERE fileid=?")

    for( fileid <- removelist ) {
      //resetLasthit.setLong(1, nowtime)
      //resetLasthit.setString(2, fileid)
      //resetLasthit.executeUpdate()

      deleteCachedFile.setString(1, fileid)
      deleteCachedFile.executeUpdate()
      HVFile.getHVFileFromFileid(fileid).getLocalFileRef().delete()
      Out.debug("Removed old static range file " + fileid)
    }

    Out.info("CacheHandler: Resetting remaining far-future lasthits...")

    val resetFutureStatic = sqlite.prepareStatement("UPDATE CacheList SET lasthit=? WHERE lasthit>?")

    resetFutureStatic.setLong(1, nowtime + 7776000)
    resetFutureStatic.setLong(2, nowtime + 31536000)
    resetFutureStatic.executeUpdate()

    sqlite.setAutoCommit(true)
  }

  def terminateDatabase() {
    if(sqlite != null) {
      try {
        setStringVar.setString(1, CLEAN_SHUTDOWN_KEY)
        setStringVar.setString(2, CLEAN_SHUTDOWN_VALUE)
        setStringVar.executeUpdate()

        sqlite.close()
      } catch{case e:Exception =>}

      sqlite = null
    }
  }

  def initializeCacheHandler() /*throws java.io.IOException*/ {
    Out.info("CacheHandler: Initializing the cache system...")

    // delete orphans from the temp dir

    val tmpfiles = CacheHandler.tmpdir.listFiles()

    for(tmpfile <- tmpfiles) {
      if(tmpfile.isFile()) {
        Out.debug("Deleted orphaned temporary file " + tmpfile)
        tmpfile.delete()
      } else {
        Out.warning("Found a non-file " + tmpfile + " in the temp directory, won't delete.")
      }
    }

    if(quickStart && !Settings.isVerifyCache()) {
      try {
        val rs = cacheIndexCountStats.executeQuery()
        if(rs.next()) {
          cacheCount = rs.getInt(1)
          cacheSize = rs.getLong(2)
        }
        rs.close()
      } catch{case e:Exception => {
        Out.error("CacheHandler: Failed to perform database operation")
        HentaiAtHomeClient.dieWithError(e)
      }}

      updateStats()
      flushRecentlyAccessed()
    } else {
      if(Settings.isVerifyCache()) {
        Out.info("CacheHandler: A full cache verification has been requested. This can take quite some time.")
      }
      populateInternalCacheTable()
    }

    if( !Settings.isSkipFreeSpaceCheck() && (CacheHandler.cachedir.getFreeSpace() < Settings.getDiskLimitBytes() - cacheSize) ) {
      // note: if this check is removed and the client ends up being starved on disk space with static ranges assigned, it will cause a major loss of trust.
      client.setFastShutdown()
      HentaiAtHomeClient.dieWithError("The storage device does not have enough space available to hold the given cache size.\nFree up space, or reduce the cache size from the H@H settings page.\nhttp://g.e-hentai.org/hentaiathome.php?cid=" + Settings.getClientID())
    }

    if( (cacheCount < 1) && (Settings.getStaticRangeCount() > 0) ) {
      // note: if this check is removed and the client is started with an empty cache and several static ranges assigned, it will cause a major loss of trust.
      client.setFastShutdown()
      HentaiAtHomeClient.dieWithError("This client has static ranges assigned to it, but the cache is empty.\nCheck permissions and, if necessary, delete the file hath.db in the data directory to rebuild the cache database.\nIf the cache has been deleted or is otherwise lost, you have to manually reset your static ranges from the H@H settings page.\nhttp://g.e-hentai.org/hentaiathome.php?cid=" + Settings.getClientID())
    }

    if(!checkAndFreeDiskSpace(CacheHandler.cachedir, true)) {
      Out.warning("ClientHandler: There is not enough space left on the disk to add more files to the cache.")
    }
  }

  def getHVFile(fileid:String, hit:Boolean):HVFile =
    if(!HVFile.isValidHVFileid(fileid)) null else {
      val cf = new CachedFile(fileid)

      if(hit) {
        cf.hit()
      }

      cf.getHVFile()
    }

  // note: this will just move the file into its correct location. addFileToActiveCache MUST be called afterwards to import the file into the necessary datastructures.
  // otherwise, the file will not be available until the client is restarted, and even then not if --quickstart is used.
  def moveFileToCacheDir(file:File, hvFile:HVFile):Boolean = {
    if(checkAndFreeDiskSpace(file)) {
      val toFile = hvFile.getLocalFileRef()

      try {
        FileTools.checkAndCreateDir(toFile.getParentFile())

        if(file.renameTo(toFile)) {
          Out.debug("CacheHandler: Imported file " + file + " to " + hvFile.getFileid())
          return true
        }
        else if(FileTools.copy(file, toFile)) {
          // rename can fail in some cases, like when source and target are on different file systems.
          // when this happens, we just use our own copy function instead, and delete the old file afterwards.
          file.delete()
          Out.debug("CacheHandler: Imported file " + file + " to " + hvFile.getFileid())
          return true
        } else {
          Out.warning("CacheHandler: Failed to move file " + file)
        }
      } catch{case e:java.io.IOException => {
        e.printStackTrace()
        Out.warning("CacheHandler: Encountered exception " + e + " when moving file " + file)
      }}
    }

    return false
  }

  def addFileToActiveCache(hvFile:HVFile) {
    try {
      sqlite.synchronized {
        val fileid = hvFile.getFileid()

        updateCachedFileActive.setString(1, fileid)
        val affected = updateCachedFileActive.executeUpdate()

        if(affected == 0) {
          var lasthit = Math.floor(System.currentTimeMillis() / 1000).toLong

          if(Settings.isStaticRange(fileid)) {
            // if the file is in a static range, bump to three months in the future. on the next access, it will get bumped further to a year.
            lasthit += 7776000
          }

          insertCachedFile.setString(1, fileid)
          insertCachedFile.setLong(2, lasthit)
          insertCachedFile.setInt(3, hvFile.getSize())
          insertCachedFile.executeUpdate()
        }
      }
    } catch{case e:Exception => {
      Out.error("CacheHandler: Failed to perform database operation")
      HentaiAtHomeClient.dieWithError(e)
    }}

    cacheCount += 1
    cacheSize += hvFile.getSize()
    updateStats()
  }

  // During server-initiated file distributes and proxy tests against other clients, the file is automatically registered for this client by the server,
  // but this doesn't happen during client-initiated H@H Downloader or H@H Proxy downloads.
  // So we'll instead send regular updates to the server about downloaded files, whenever a file is added this way.
  def addPendingRegisterFile(hvFile:HVFile) {
    // We only register files <= 10 MB. Larger files are handled outside the H@H network.
    if( (hvFile.getSize() <= 10485760) && !Settings.isStaticRange(hvFile.getFileid()) ) {
      pendingRegister.synchronized {
        Out.debug("Added " + hvFile + " to pendingRegister")
        pendingRegister :+= hvFile

        if(pendingRegister.size >= 50) {
          // this call also empties the list
          client.getServerHandler().notifyRegisterFiles(pendingRegister)
          pendingRegister = List()
        }
      }
    } else {
      Out.debug("Not registering file " + hvFile + " - in static range or larger than 10 MB")
    }
  }

  def deleteFileFromCache(toRemove:HVFile) {
    sqlite.synchronized {
      deleteFileFromCacheNosync(toRemove)
    }
  }

  private def deleteFileFromCacheNosync(toRemove:HVFile) {
    try {
      deleteCachedFile.setString(1, toRemove.getFileid())
      deleteCachedFile.executeUpdate()
      cacheCount -= 1
      cacheSize -= toRemove.getSize()
      toRemove.getLocalFileRef().delete()
      Out.info("CacheHandler: Deleted cached file " + toRemove.getFileid())
      updateStats()
    } catch{case e:Exception => {
        Out.error("CacheHandler: Failed to perform database operation")
        HentaiAtHomeClient.dieWithError(e)
    }}
  }

  private def populateInternalCacheTable() {
    try {
        cacheIndexClearActive.executeUpdate()

        cacheCount = 0
        cacheSize = 0

        var knownFiles = 0
        var newFiles = 0

        // load all the files directly from the cache directory itself and initialize the stored last access times for each file. last access times are used for the LRU-style cache.

        Out.info("CacheHandler: Loading cache.. (this could take a while)")

        var scdirs = CacheHandler.cachedir.listFiles().sorted
        //java.util.Arrays.sort(scdirs)

        try {
            // we're doing some SQLite operations here without synchronizing on the SQLite connection. the program is single-threaded at this point, so it should not be a real problem.

            var loadedFiles = 0
            sqlite.setAutoCommit(false)

            for(scdir <- scdirs) {
              if(scdir.isDirectory()) {
                var cfiles = scdir.listFiles().sorted
                //java.util.Arrays.sort(cfiles)

                for(cfile <- cfiles) {
                  var newFile = false

                  sqlite.synchronized {
                    queryCachedFileLasthit.setString(1, cfile.getName())
                    val rs = queryCachedFileLasthit.executeQuery()
                    newFile = !rs.next()
                    rs.close()
                  }

                  val hvFile = HVFile.getHVFileFromFile(cfile, Settings.isVerifyCache() || newFile)

                  if(hvFile != null) {
                    addFileToActiveCache(hvFile)

                    if(newFile) {
                      newFiles += 1
                      Out.info("CacheHandler: Verified and loaded file " + cfile)
                    } else {
                      knownFiles += 1
                    }

                    loadedFiles += 1
                    if(loadedFiles % 1000 == 0) {
                      Out.info("CacheHandler: Loaded " + loadedFiles + " files so far...")
                    }
                  } else {
                    Out.warning("CacheHandler: The file " + cfile + " was corrupt. It is now deleted.")
                    cfile.delete()
                  }
                }
              } else {
                scdir.delete()
              }
              flushRecentlyAccessed(false)
            }

            sqlite.commit()
            sqlite.setAutoCommit(true)

            sqlite.synchronized {
                val purged = deleteCachedFileInactive.executeUpdate()
                Out.info("CacheHandler: Purged " + purged + " nonexisting files from database.")
            }
        } catch{case e:Exception => {
          Out.error("CacheHandler: Failed to perform database operation")
          HentaiAtHomeClient.dieWithError(e)
        }}

        Out.info("CacheHandler: Loaded " + knownFiles + " known files.")
        Out.info("CacheHandler: Loaded " + newFiles + " new files.")
        Out.info("CacheHandler: Finished initializing the cache (" + cacheCount + " files, " + cacheSize + " bytes)")

        updateStats()
    } catch{case e:Exception => {
      e.printStackTrace()
      HentaiAtHomeClient.dieWithError("Failed to initialize the cache.")
    }}
  }

  def recheckFreeDiskSpace():Boolean = checkAndFreeDiskSpace(CacheHandler.cachedir)
  private def checkAndFreeDiskSpace(file:File):Boolean = checkAndFreeDiskSpace(file, false)
  private def checkAndFreeDiskSpace(file:File, noServerDeleteNotify:Boolean):Boolean = {
    this.synchronized {
      if(file == null) {
          HentaiAtHomeClient.dieWithError("CacheHandler: checkAndFreeDiskSpace needs a file handle to calculate free space")
      }

      val bytesNeeded = if(file.isDirectory()) 0 else file.length().toInt
      val cacheLimit = Settings.getDiskLimitBytes()

      Out.debug("CacheHandler: Checking disk space (adding " + bytesNeeded + " bytes: cacheSize=" + cacheSize + ", cacheLimit=" + cacheLimit + ", cacheFree=" + (cacheLimit - cacheSize) + ")")

      // we'll free ten times the size of the file or 20 files, whichever is largest.

      var bytesToFree:Long = 0

      if(cacheSize > cacheLimit) {
          bytesToFree = cacheSize - cacheLimit
      }
      else if(cacheSize + bytesNeeded - cacheLimit > 0) {
          bytesToFree = bytesNeeded * 10
      }

      if( bytesToFree > 0 ) {
        Out.info("CacheHandler: Freeing at least " + bytesToFree + " bytes...")
        //List<HVFile> deleteNotify = Collections.checkedList(new ArrayList<HVFile>(), HVFile.class)
        var deleteNotify:List[HVFile] = List()

        try {
          while( bytesToFree > 0 && cacheCount > 0 ) {
            sqlite.synchronized {
              queryCachedFileSortOnLasthit.setInt(1,  0)
              queryCachedFileSortOnLasthit.setInt(2, 20)

              val rs = queryCachedFileSortOnLasthit.executeQuery()

              while( rs.next() ) {
                val fileid = rs.getString(1)
                val toRemove = HVFile.getHVFileFromFileid(fileid)

                if( toRemove != null ) {
                  deleteFileFromCacheNosync(toRemove)
                  bytesToFree -= toRemove.getSize()

                  if( !Settings.isStaticRange(fileid) ) {
                    // don't notify for static range files
                    deleteNotify :+= toRemove
                  }
                }
              }

              rs.close()
            }
          }
        } catch{case e: Exception => {
          Out.error("CacheHandler: Failed to perform database operation")
          HentaiAtHomeClient.dieWithError(e)
        }}

        if( !noServerDeleteNotify ) {
          client.getServerHandler().notifyUncachedFiles(deleteNotify)
        }
      }

      if(Settings.isSkipFreeSpaceCheck()) {
          Out.debug("CacheHandler: Disk free space check is disabled.")
          return true
      } else {
          val diskFreeSpace = file.getFreeSpace()

          if(diskFreeSpace < Math.max(Settings.getDiskMinRemainingBytes(), 104857600)) {
              // if the disk fills up, we  stop adding files instead of starting to remove files from the cache, to avoid being unintentionally squeezed out by other programs
              Out.warning("CacheHandler: Cannot meet space constraints: Disk free space limit reached (" + diskFreeSpace + " bytes free on device)")
              return false
          }
          else {
              Out.debug("CacheHandler: Disk space constraints met (" + diskFreeSpace + " bytes free on device)")
              return true
          }
      }
    }
  }

  def pruneOldFiles() {
    this.synchronized {
      //List<HVFile> deleteNotify = Collections.checkedList(new ArrayList<HVFile>(), HVFile.class)
      var deleteNotify:List[HVFile] = List()
      var pruneCount = 0

      Out.info("Checking for old files to prune...")

      try {
        sqlite.synchronized {
          queryCachedFileSortOnLasthit.setInt(1,  0)
          queryCachedFileSortOnLasthit.setInt(2, 20)

          val rs = queryCachedFileSortOnLasthit.executeQuery()
          val nowtime = Math.floor(System.currentTimeMillis() / 1000).toLong

          while( rs.next() ) {
            val fileid = rs.getString(1)
            val lasthit = rs.getInt(2)

            if( lasthit < nowtime - 2592000 ) {
              val toRemove = HVFile.getHVFileFromFileid(fileid)

              if( toRemove != null ) {
                deleteFileFromCacheNosync(toRemove)
                pruneCount += 1

                if( !Settings.isStaticRange(fileid) ) {
                  // don't notify for static range files
                  deleteNotify :+= toRemove
                }
              }
            }
          }

          rs.close()
        }
      }
      catch{case e:Exception =>{
        Out.error("CacheHandler: Failed to perform database operation")
        HentaiAtHomeClient.dieWithError(e)
      }}

      client.getServerHandler().notifyUncachedFiles(deleteNotify)

      Out.info("Pruned " + pruneCount + " files.")
    }
  }

  def processBlacklist(deltatime:Long, noServerDeleteNotify:Boolean) {
    this.synchronized {
      Out.info("CacheHandler: Retrieving list of blacklisted files...")
      val blacklisted = client.getServerHandler().getBlacklist(deltatime)

      if(blacklisted == null) {
          Out.warning("CacheHandler: Failed to retrieve file blacklist, will try again later.")
          return
      }

      Out.info("CacheHandler: Looking for and deleting blacklisted files...")

      var counter = 0
      //val deleteNotify = Collections.checkedList(new ArrayList<HVFile>(), HVFile.class)
      var deleteNotify:List[HVFile] = List()

      try {
        sqlite.synchronized {
          for(fileid <- blacklisted) {
            queryCachedFileLasthit.setString(1, fileid)
            val rs = queryCachedFileLasthit.executeQuery()
            var toRemove:HVFile = null

            if(rs.next()) {
              toRemove = HVFile.getHVFileFromFileid(fileid)
            }

            rs.close()

            if(toRemove != null) {
              //Out.info("CacheHandler: Removing blacklisted file " + fileid)
              counter += 1
              deleteFileFromCacheNosync(toRemove)

              if(!Settings.isStaticRange(toRemove.getFileid())) {
                // do not notify about static range files
                deleteNotify :+= toRemove
              }
            }
          }
        }
      } catch{case e:Exception => {
        Out.error("CacheHandler: Failed to perform database operation")
        HentaiAtHomeClient.dieWithError(e)
      }}

      if(!noServerDeleteNotify) {
        client.getServerHandler().notifyUncachedFiles(deleteNotify)
      }

      Out.info("CacheHandler: " + counter + " blacklisted files were removed.")
    }
  }

  private def updateStats() {
    Stats.setCacheCount(cacheCount)
    Stats.setCacheSize(cacheSize)
  }

  def getCacheCount() = cacheCount

  def getSegmentCount() =
      if((Settings.isUseLessMemory() && cacheCount > 16000) || (cacheCount > 400000)) 256 else 16

  def getStartupCachedFilesStrlen() = startupCachedFileStrlen

  def calculateStartupCachedFilesStrlen() {
      val segmentCount = getSegmentCount()
      startupCachedFileStrlen = 0

      (0 to segmentCount-1).foreach({(segmentIndex:Int) =>
        startupCachedFileStrlen += getCachedFilesSegment((segmentCount | segmentIndex)
          .toHexString.substring(1)).map(_.length()+1).sum

        //for( String fileid : fileList ) {
          //startupCachedFileStrlen += fileid.length() + 1
        //}

        Out.info("Calculated segment " + segmentIndex + " of " + segmentCount)
      })
  }

  def getCachedFilesSegment(segment:String):List[String] = {
      var fileList:List[String] = List()

      try {
        System.gc()

        sqlite.synchronized {
          queryCachelistSegment.setString(1, segment + "0")
          queryCachelistSegment.setString(2, segment + "g")
          val rs = queryCachelistSegment.executeQuery()

          while( rs.next() ) {
            val fileid = rs.getString(1)
            if( !Settings.isStaticRange(fileid) ) {
              fileList :+= fileid
            }
          }

          rs.close()
        }

        System.gc()
      } catch { case e:Exception => {
        Out.error("CacheHandler: Failed to perform database operation")
        HentaiAtHomeClient.dieWithError(e)
      }}

      fileList
  }

  def flushRecentlyAccessed() {
    flushRecentlyAccessed(true)
  }
  private def flushRecentlyAccessed(disableAutocommit:Boolean) {
    this.synchronized {
      var flushCheck:List[CachedFile] = null
      var flush:List[CachedFile] = null

      if(memoryWrittenTable != null) {
        // this function is called every 10 seconds. clearing 121 of the shorts for each call means that each element will live up to a day (since 1048576 / 8640 is roughly 121).
        // note that this is skipped if the useLessMemory flag is set.

        val clearUntil = Math.min(MEMORY_TABLE_ELEMENTS, memoryClearPointer + 121)

        //Out.debug("CacheHandler: Clearing memoryWrittenTable from " + memoryClearPointer + " to " + clearUntil)

        while(memoryClearPointer < clearUntil) {
          memoryWrittenTable(memoryClearPointer) = 0
          memoryClearPointer+=1
        }

        if(clearUntil >= MEMORY_TABLE_ELEMENTS) {
          memoryClearPointer = 0
        }
      }

      recentlyAccessed.synchronized {
        recentlyAccessedFlush = System.currentTimeMillis()
        //flushCheck = new ArrayList<CachedFile>(recentlyAccessed.size())
        //flushCheck.addAll(recentlyAccessed)
        flushCheck = recentlyAccessed
        //recentlyAccessed.clear()
        recentlyAccessed = List()
      }

      if(flushCheck.size > 0) {
        try {
          sqlite.synchronized {
            flush = List() //= new ArrayList<CachedFile>(flushCheck.size())

            for(cf <- flushCheck) {
              val fileid = cf.getFileid()
              var doFlush = true

              if(memoryWrittenTable != null) {
                // if the memory table is active, we use this as a first step in order to determine if the timestamp should be updated or not.
                // we first need to compute the array index and bitmask for this particular fileid.
                // then, if the bit is set, we do not update. if not, we update but set the bit.

                doFlush = false

                try {
                  val arrayIndex:Int = (0 to 4).map({
                    (i:Int) => Integer.parseInt(fileid.substring(i, i+1), 16) << ((4 - i) * 4)
                  }).sum

                  val bitMask:Short = (1 << Integer.parseInt(fileid.substring(5, 6), 16)).toShort
                  val value:Short = memoryWrittenTable(arrayIndex)

                  if( (value & bitMask) != 0) {
                    //Out.debug("Written bit for " + fileid + " = " + arrayIndex + ":" + fileid.charAt(5) + " was set")
                  } else {
                    //Out.debug("Written bit for " + fileid + " = " + arrayIndex + ":" + fileid.charAt(5) + " was not set - flushing")
                    memoryWrittenTable(arrayIndex) = (value | bitMask).toShort
                    doFlush = true
                  }
                } catch{case e:Exception =>
                  Out.warning("Encountered invalid fileid " + fileid + " while checking memoryWrittenTable.")
                }
              }

              if(doFlush) {
                // we don't need higher resolution than a day for the LRU mechanism, so we'll save expensive writes by not updating timestamps for files that have been flagged the previous 24 hours.
                // (reads typically don't involve an actual disk access as the database file is cached to RAM - writes always do unless it can be combined with another write)

                queryCachedFileLasthit.setString(1, fileid)
                val rs = queryCachedFileLasthit.executeQuery()

                if( rs.next() ) {
                  val hittime = rs.getLong(1)
                  val nowtime = Math.floor(System.currentTimeMillis() / 1000).toLong

                  if( Settings.isStaticRange(fileid) ) {
                    // for static range files, do not flush if it was already flushed this month
                    // static range files have hittime set to nowtime + 31536000 every time they flush
                    // so if hittime is less than nowtime + 28944000, flush it
                    if( hittime > nowtime + 28944000 ) {
                      doFlush = false
                    }
                  } else {
                    // for other files, do not flush if it was already flushed today
                    if( hittime > nowtime - 86400 ) {
                      doFlush = false
                    }
                  }
                }

                if( doFlush ) {
                  flush :+= cf
                }

                rs.close()
              }
            }

            if(flush.size > 0) {
              if(disableAutocommit) {
                sqlite.setAutoCommit(false)
              }

              for(cf <- flush) {
                if(cf.needsFlush()) {
                  val fileid = cf.getFileid()
                  var lasthit = Math.floor(System.currentTimeMillis() / 1000).toLong

                  if(Settings.isStaticRange(fileid)) {
                    // if the file is in a static range, bump to one year in the future
                    lasthit += 31536000
                  }

                  updateCachedFileLasthit.setLong(1, lasthit)
                  updateCachedFileLasthit.setString(2, fileid)
                  updateCachedFileLasthit.executeUpdate()

                  // there is a race condition here of sorts, but it doesn't matter. flushed() will set needFlush to false, which can be set to true by hit(),
                  // but no matter the end result we have an acceptable outcome. (it's always flushed at least once.)
                  cf.flushed()
                }
              }

              if(disableAutocommit) {
                sqlite.setAutoCommit(true)
              }
            }
          }
        } catch{case e: Exception => {
          Out.error("CacheHandler: Failed to perform database operation")
          HentaiAtHomeClient.dieWithError(e)
        }}
      }
    }
  }

  class CachedFile(fileid:String) {
    private var needFlush = false

    def getFileid() = fileid
    def getHVFile() = HVFile.getHVFileFromFileid(fileid)
    def needsFlush() = needFlush

    def flushed() {
      needFlush = false
    }
    def hit() {
      recentlyAccessed.synchronized {
        needFlush = true
        recentlyAccessed :+= this
      }
    }
  }
}

object CacheHandler {
  private var cachedir:File = null
  private var tmpdir:File = null
  def getCacheDir() = cachedir
  def getTmpDir() = tmpdir
}
