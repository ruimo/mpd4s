package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.{LsInfoEntry, ResponseException}

class LoadSpec extends Specification {
  "Load" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        conn.load("test", None)
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "load test\n"
    }

    "Can handle ok with args" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        conn.load("test", Some(1,2))
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "load test 1:2\n"
    }

    "Can handle fail" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + """ACK [5@0] {} unknown command "fb"\n"""
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      try {
        new Mpc(() => socket).withConnection { conn =>
          conn.version === Version("0.19.0")
          conn.load("test", None)
          failure
        }
      } catch {
        case e: ResponseException =>
          e.errorNo === 5
          e.commandIdx === 0
          e.command === ""
          e.message === """unknown command "fb"\n"""
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "load test\n"
    }
  }
}
