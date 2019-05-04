package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response._
import scala.collection.{immutable => imm}

class PlayListInfoSpec extends Specification {
  "Play list info" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "file: Beethoven/MissaSolemnis/Karajan/track3.flac\n"
          + "Last-Modified: 2012-11-24T04:16:41Z\n"
          + "Time: 1333\n"
          + "Pos: 0\n"
          + "Id: 68\n"
          + "file: Beethoven/MissaSolemnis/Karajan/Track4.flac\n"
          + "Last-Modified: 2012-11-24T04:16:47Z\n"
          + "Time: 223\n"
          + "Pos: 1\n"
          + "Id: 69\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val pl: imm.Seq[PlayListInfoEntry] = conn.playListInfo().entries
        pl.size === 2
        pl(0).id === 68
        pl(0).lastModified === Instant.parse("2012-11-24T04:16:41Z")
        pl(0).length === 1333
        pl(0).path === "Beethoven/MissaSolemnis/Karajan/track3.flac"
        pl(0).pos === 0

        pl(1).id === 69
        pl(1).lastModified === Instant.parse("2012-11-24T04:16:47Z")
        pl(1).length === 223
        pl(1).path === "Beethoven/MissaSolemnis/Karajan/Track4.flac"
        pl(1).pos === 1
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "playlistinfo\n"
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
        conn.playListInfo().entries.size === 0
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "playlistinfo\n"
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
          conn.playListInfo()
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
      new String(out.toByteArray, "utf-8") === "playlistinfo\n"
    }
  }
}
