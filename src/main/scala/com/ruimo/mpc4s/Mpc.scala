package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket
import com.ruimo.scoins.LoanPattern._
import java.io.{InputStream, OutputStream}

class Mpc(socketFactory: () => Socket) {
  def withConnection[T](f: (InputStream, OutputStream) => T): T = using(socketFactory()) { socket =>
    f(socket.getInputStream, socket.getOutputStream)
  }.get
}

object Mpc {
  def apply(host: InetAddress, port: Int = 6600): Mpc = new Mpc(() => new Socket(host, port))
}
