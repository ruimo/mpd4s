package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket
import com.ruimo.scoins.LoanPattern._
import java.io.{InputStream, OutputStream, BufferedReader, InputStreamReader, BufferedWriter, OutputStreamWriter}

case class Version(value: String)

class Mpc(socketFactory: () => Socket) {
  import Mpc._

  def withConnection[T](f: Connection => T): T = using(socketFactory()) { socket =>
    val in: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream, "utf-8"))
    val conn = new ConnectionImpl(version(in), in, new BufferedWriter(new OutputStreamWriter(socket.getOutputStream, "utf-8")))
    f(conn)
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
    def clearError(): Response.ClearError = {
      Request.clearError.writeln(out)
      Response.clearError(in)
    }
  }
}
