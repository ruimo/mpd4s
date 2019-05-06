package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.{LsInfoEntry, ResponseException}

class RenameSpec extends Specification {
  "Rename" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        conn.rename("test", "test2")
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "rename test test2\n"
    }

    "Can handle fail" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + """ACK [56@0] {save} Playlist already exists\n"""
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      try {
        new Mpc(() => socket).withConnection { conn =>
          conn.version === Version("0.19.0")
          conn.rename("test", "test2")
          failure
        }
      } catch {
        case e: ResponseException =>
          e.errorNo === 56
          e.commandIdx === 0
          e.command === "save"
          e.message === """Playlist already exists\n"""
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "rename test test2\n"
    }
  }
}
