package com.ruimo.mpc4s

import java.io.{InputStream, OutputStream}
import java.net.Socket

class MockSocket(
  in: InputStream, out: OutputStream
) extends Socket {
  var closed = false

  override def getInputStream(): InputStream = in
  override def getOutputStream(): OutputStream = out
  override def close(): Unit = {
    closed = true
  }
}
