package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.{PlayState, ResponseException, StatusInfo}

class StatusSpec extends Specification {
  "Status" should {
    "Can handle ok" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "volume: -1\n"
          + "repeat: 0\n"
          + "random: 0\n"
          + "single: 0\n"
          + "consume: 0\n"
          + "playlist: 65\n"
          + "playlistlength: 2\n"
          + "mixrampdb: 0.000000\n"
          + "state: stop\n"
          + "song: 0\n"
          + "songid: 65\n"
          + "nextsong: 1\n"
          + "nextsongid: 66\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val status: StatusInfo = conn.status()
        status.volume === None
        status.repeat === false
        status.random === false
        status.single === Response.SinglePlay.No
        status.consume === false
        status.playList === 65
        status.playListLength === 2
        status.mixRampDb.get must beCloseTo(0, 0.1)
        status.state === PlayState.Stop
        status.song === Some(0)
        status.songId === Some(65)
        status.nextSong === Some(1)
        status.nextSongId === Some(66)
        status.elapsed === None
        status.duration === None
        status.bitRate === None
        status.xfade === None
        status.mixRampDelay === None
        status.audio === None
        status.updatingDb === None
        status.error === None
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "status\n"
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
          conn.status()
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
      new String(out.toByteArray, "utf-8") === "status\n"
    }
  }
}
