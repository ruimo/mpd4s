package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.ResponseException

class ListPlayListsSpec extends Specification {
  "List playlist" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "playlist: test\n"
          + "Last-Modified: 2019-04-27T09:23:12Z\n"
          + "playlist: ChopinPolonaisrOp22\n"
          + "Last-Modified: 2019-04-28T11:45:20Z\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val entries = conn.listPlaylists().entries
        entries.size === 2
        entries(0).name === "test"
        entries(0).lastModified === Instant.parse("2019-04-27T09:23:12Z")
        entries(1).name === "ChopinPolonaisrOp22"
        entries(1).lastModified === Instant.parse("2019-04-28T11:45:20Z")
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "listplaylists\n"
    }

    "Can handle empty" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val entries = conn.listPlaylists().entries
        entries.size === 0
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "listplaylists\n"
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
          conn.listPlaylists()
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
      new String(out.toByteArray, "utf-8") === "listplaylists\n"
    }
  }
}
