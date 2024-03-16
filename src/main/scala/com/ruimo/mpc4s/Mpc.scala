package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

import java.io._

import org.slf4j.LoggerFactory
import org.slf4j.Logger
import java.io.FilterReader
import scala.util.Using

case class Version(value: String)

class Mpc(socketFactory: () => Socket) {
  import Mpc._
  val logger = LoggerFactory.getLogger(getClass)

  private class LoggingReader(in: Reader, buf: StringBuilder) extends FilterReader(in) {
    override def read(): Int = {
      val c = super.read()
      if (c != -1) {
        buf.append(c.asInstanceOf[Char])
      }
      c
    }

    override def read(cbuf: Array[Char], off: Int, len: Int): Int = {
      val ret = super.read(cbuf, off, len)
      if (0 < ret) {
        buf.appendAll(cbuf, off, ret)
      }
      ret
    }
  }

  private class LoggingWriter(out: Writer, buf: StringBuilder) extends FilterWriter(out) {
    override def write(c: Int): Unit = {
      buf.append(c.asInstanceOf[Char])
      super.write(c)
    }

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
      buf.appendAll(cbuf, off, len)
      super.write(cbuf, off, len)
    }

    override def write(str: String, off: Int, len: Int): Unit = {
      buf.append(str.substring(off, len))
      super.write(str, off, len)
    }
  }

  def withConnection[T](f: Connection => T): T = Using.resource(socketFactory()) { socket =>
    val readerLog = new StringBuilder()
    val writerLog = new StringBuilder()
    val in: BufferedReader = new BufferedReader(
      {
        val reader = new InputStreamReader(socket.getInputStream, "utf-8")
        if (logger.isDebugEnabled()) new LoggingReader(reader, readerLog) else reader
      }
    )
    val out: Writer = {
      val writer = new OutputStreamWriter(socket.getOutputStream, "utf-8")
      if (logger.isDebugEnabled()) new LoggingWriter(writer, writerLog) else writer
    }
    val conn = new ConnectionImpl(version(in), in, new BufferedWriter(out))
    try {
      f(conn)
    } finally {
      if (logger.isErrorEnabled()) {
        logger.debug("Mpd request: '" + writerLog.toString + "'")
        logger.debug("Mpd response: '" + readerLog.toString + "'")
      }
    }
  }

  def withBatchConnection(f: BatchConnection => Unit): Unit = Using.resource(socketFactory()) { socket =>
    val readerLog = new StringBuilder()
    val writerLog = new StringBuilder()
    val in: BufferedReader = new BufferedReader(
      {
        val reader = new InputStreamReader(socket.getInputStream, "utf-8")
        if (logger.isDebugEnabled()) new LoggingReader(reader, readerLog) else reader
      }
    )
    val out: BufferedWriter = {
      val writer = new OutputStreamWriter(socket.getOutputStream, "utf-8")
      new BufferedWriter(if (logger.isDebugEnabled()) new LoggingWriter(writer, writerLog) else writer)
    }
    try {
      out.write("command_list_begin\n")
      val conn = new BatchConnectionImpl(version(in), in, out)
      f(conn)
      out.write("command_list_end\n")
      out.flush()
      Response.batchResult(in)
    } finally {
      if (logger.isErrorEnabled()) {
        logger.debug("Mpd request: '" + writerLog.toString + "'")
        logger.debug("Mpd response: '" + readerLog.toString + "'")
      }
    }
  }
}

object Mpc {
  val VersionPattern = "OK MPD (.*)".r

  def apply(host: InetAddress, port: Int = 6600): Mpc = new Mpc(() => new Socket(host, port))
  def byHostName(hostName: String, port: Int = 6600): Mpc = this(InetAddress.getByName(hostName), port)

  private def version(in: BufferedReader): Version = in.readLine() match {
    case VersionPattern(ver) => Version(ver)
    case l @ _ => throw new IllegalStateException("Invalid version string '" + l + "'")
  }

  private class ConnectionImpl(
    val version: Version,
    val in: BufferedReader,
    val out: BufferedWriter
  ) extends Connection {
    override def clearError(): Unit = {
      Request.clearError.writeln(out)
      Response.clearError(in)
    }

    override def stop(): Unit = {
      Request.stop.writeln(out)
      Response.stop(in)
    }

    override def clear(): Unit = {
      Request.clear.writeln(out)
      Response.clear(in)
    }

    override def lsInfo(path: Option[String]): Response.LsInfo = {
      Request.lsInfo(path).writeln(out)
      Response.lsInfo(in)
    }

    override def add(path: String): Unit = {
      Request.add(path).writeln(out)
      Response.add(in)
    }

    override def play(idx: Option[Int]): Unit = {
      Request.play(idx).writeln(out)
      Response.play(in)
    }

    override def status(): Response.StatusInfo = {
      Request.status.writeln(out)
      Response.status(in)
    }

    override def currentSong(): Option[Response.SongInfo] = {
      Request.currentSong.writeln(out)
      Response.currentSong(in)
    }

    override def pause(): Unit = {
      Request.pause.writeln(out)
      Response.pause(in)
    }

    override def playlistInfo(): Response.PlaylistInfo = {
      Request.playlistInfo.writeln(out)
      Response.playlistInfo(in)
    }

    override def listPlaylists(): Response.StoredPlaylistInfo = {
      Request.listPlaylists.writeln(out)
      Response.listPlaylists(in)
    }

    override def load(name: String, range: Option[(Int, Int)] = None): Unit = {
      Request.load(name, range).writeln(out)
      Response.load(in)
    }

    override def deleteId(id: Int): Unit = {
      Request.deleteId(id).writeln(out)
      Response.deleteId(in)
    }

    override def moveId(fromId: Int, toIndex: Int): Unit = {
      Request.moveId(fromId, toIndex).writeln(out)
      Response.moveId(in)
    }

    override def save(name: String): Unit = {
      Request.save(name).writeln(out)
      Response.save(in)
    }

    override def rm(name: String): Unit = {
      Request.rm(name).writeln(out)
      Response.rm(in)
    }

    override def rename(name: String, newName: String): Unit = {
      Request.rename(name, newName).writeln(out)
      Response.rename(in)
    }

    override def playId(id: Int): Unit = {
      Request.playId(id).writeln(out)
      Response.playId(in)
    }

    override def update(uri: Option[String]): Unit = {
      Request.update(uri).writeln(out)
      Response.update(in)
    }
  }

  private class BatchConnectionImpl(
    val version: Version,
    val in: BufferedReader,
    val out: BufferedWriter
  ) extends BatchConnection {
    override def clearError(): BatchConnection = {
      Request.clearError.writeln(out)
      this
    }

    override def stop(): BatchConnection = {
      Request.stop.writeln(out)
      this
    }

    override def clear(): BatchConnection = {
      Request.clear.writeln(out)
      this
    }

    override def add(path: String): BatchConnection = {
      Request.add(path).writeln(out)
      this
    }

    override def play(idx: Option[Int]): BatchConnection = {
      Request.play(idx).writeln(out)
      this
    }
  
    override def pause(): BatchConnection = {
      Request.pause.writeln(out)
      this
    }

    override def load(name: String, range: Option[(Int, Int)] = None): BatchConnection = {
      Request.load(name, range).writeln(out)
      this
    }

    override def deleteId(id: Int): BatchConnection = {
      Request.deleteId(id).writeln(out)
      this
    }

    override def moveId(fromId: Int, toIndex: Int): BatchConnection = {
      Request.moveId(fromId, toIndex).writeln(out)
      this
    }

    override def save(name: String): BatchConnection = {
      Request.save(name).writeln(out)
      this
    }

    override def rm(name: String): BatchConnection = {
      Request.rm(name).writeln(out)
      this
    }

    override def rename(name: String, newName: String): BatchConnection = {
      Request.rename(name, newName).writeln(out)
      this
    }

    override def playId(id: Int): BatchConnection = {
      Request.playId(id).writeln(out)
      this
    }

    override def update(uri: Option[String]): BatchConnection = {
      Request.update(uri).writeln(out)
      this
    }
  }
}
