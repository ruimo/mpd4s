package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

class Connection(host: InetAddress, port: Int = 6600) extends AutoCloseable {
  private[this] val socket = new Socket(host, port)

  def close() {
    socket.close()
  }
}
