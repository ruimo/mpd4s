package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

import com.ruimo.scoins.LoanPattern._
import java.io._

import org.slf4j.LoggerFactory
import org.slf4j.Logger
import java.io.FilterReader

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

  def withConnection[T](f: Connection => T): T = using(socketFactory()) { socket =>
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
  }.get

  def withBatchConnection(f: BatchConnection => Unit): Unit = using(socketFactory()) { socket =>
    val in = new BufferedReader(new InputStreamReader(socket.getInputStream, "utf-8"))
    val out = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, "utf-8"))
    out.write("command_list_begin\n")
    val conn = new BatchConnectionImpl(version(in), in, out)
    f(conn)
    out.write("command_list_end\n")
    out.flush()
    Response.batchResult(in)
  }.get
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
      Request.load(name, range)
      this
    }

    override def deleteId(id: Int): BatchConnection = {
      Request.deleteId(id)
      this
    }
  }
}
