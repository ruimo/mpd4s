package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

import org.specs2.mutable.Specification

class VersionSpec extends Specification {
  "Version" should {
    "Can obtain version" in {
      val in = new ByteArrayInputStream("OK MPD 0.19.0\n".getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
      }

      socket.closed === true
      out.toByteArray.length === 0
    }
  }
}
