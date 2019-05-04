package com.ruimo.mpc4s

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream}
import java.time.Instant

import org.specs2.mutable.Specification
import com.ruimo.mpc4s.Response.{LsInfoEntry, ResponseException}

class LsInfoSpec extends Specification {
  "LsInfo" should {
    "Can handle ok when path specified" in {
      val in = new ByteArrayInputStream((
        "OK MPD 0.19.0\n"
          + "playlist: Bach/@play.m3u\n"
          + "Last-Modified: 2012-11-24T04:23:44Z\n"
          + "directory: Bach/Cantata\n"
          + "Last-Modified: 2013-06-15T02:30:30Z\n"
          + "file: incubation/Liszt-S163-12-R3.flac\n"
          + "Last-Modified: 2018-08-18T10:13:37Z\n"
          + "Time: 299\n"
          + "OK\n"
      ).getBytes("utf-8"))
      val out = new ByteArrayOutputStream()
      val socket = new MockSocket(in, out)

      new Mpc(() => socket).withConnection { conn =>
        conn.version === Version("0.19.0")
        val entries = conn.lsInfo(Some("path0")).entries
        entries.size === 3
        entries(0) === LsInfoEntry.PlayList("Bach/@play.m3u", Instant.parse("2012-11-24T04:23:44Z"))
        entries(1) === LsInfoEntry.Directory("Bach/Cantata", Instant.parse("2013-06-15T02:30:30Z"))
        entries(2) === LsInfoEntry.File("incubation/Liszt-S163-12-R3.flac", Instant.parse("2018-08-18T10:13:37Z"), 299)
      }

      socket.closed === true
      new String(out.toByteArray, "utf-8") === "lsinfo path0\n"
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
          conn.lsInfo(None)
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
      new String(out.toByteArray, "utf-8") === "lsinfo\n"
    }
  }
}
