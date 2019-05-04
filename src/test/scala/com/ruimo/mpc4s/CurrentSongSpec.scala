package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.{PlayState, ResponseException, SongInfo, StatusInfo}

class CurrentSongSpec extends Specification {
  "Current song" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "file: Beethoven/PianoSonata/Richter/No23Op57/mov02.flac\n"
          + "Last-Modified: 2012-11-24T04:14:43Z\n"
          + "Time: 358\n"
          + "Pos: 0\n"
          + "Id: 65\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val cs: SongInfo = conn.currentSong().get
        cs.id === 65
        cs.lastModified === Instant.parse("2012-11-24T04:14:43Z")
        cs.length === 358
        cs.pos === 0
        cs.path === "Beethoven/PianoSonata/Richter/No23Op57/mov02.flac"
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "currentsong\n"
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
        conn.currentSong() === None
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "currentsong\n"
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
          conn.currentSong()
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
      new String(out.toByteArray, "utf-8") === "currentsong\n"
    }
  }
}
