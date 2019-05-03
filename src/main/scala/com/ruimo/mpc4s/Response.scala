package com.ruimo.mpc4s

import java.io.BufferedReader
import java.time.Instant
import scala.collection.{immutable => imm}
import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls._
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import play.api.libs.json._

object Response {
  val logger = LoggerFactory.getLogger(getClass)

  val OkPattern = "OK".r
  val FailPattern = """ACK \[([0-9]+)@(.*)\] \{(.*)\} (.*)""".r

  class ResponseException(
    val errorNo: Int,
    val commandIdx: Int,
    val command: String,
    val message: String
  ) extends Exception

  class InternalException(msg: String = null, cause: Throwable = null) extends Exception(msg, cause)

  sealed trait LsInfoEntry {
    val path: String
    val lastModified: Instant
  }

  object LsInfoEntry {
    case class Directory(path: String, lastModified: Instant) extends LsInfoEntry
    case class File(path: String, lastModified: Instant, length: Int) extends LsInfoEntry
    case class PlayList(path: String, lastModified: Instant) extends LsInfoEntry

    implicit object Format extends Format[LsInfoEntry] {
      override def reads(jv: JsValue): JsResult[LsInfoEntry] = JsSuccess(
        (jv \ "type").as[String] match {
          case "directory" => Directory(
            (jv \ "path").as[String],
            Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong)
          )
          case "file" => File(
            (jv \ "path").as[String],
            Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong),
            (jv \ "length").as[Int]
          )
          case "playlist" => PlayList(
            (jv \ "path").as[String],
            Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong)
          )
        }
      )

      override def writes(in: LsInfoEntry): JsValue = in match {
        case Directory(path, lastModified) => Json.obj(
          "type" -> "directory",
          "path" -> path,
          "lastModified" -> lastModified.toEpochMilli.toString
        )
        case File(path, lastModified, length) => Json.obj(
          "type" -> "file",
          "path" -> path,
          "lastModified" -> lastModified.toEpochMilli.toString,
          "length" -> length
        )
        case PlayList(path, lastModified) => Json.obj(
          "type" -> "playlist",
          "path" -> path,
          "lastModified" -> lastModified.toEpochMilli.toString
        )
      }
    }
  }

  trait LsInfo {
    val info: imm.Seq[LsInfoEntry]
  }

  class Volume private (val value: Int) extends AnyVal
  object Volume {
    def apply(value: Int): Volume =
      if (value < 0 || 100 < value) throw new IllegalArgumentException("Volume(=" + value + ") should be 0 - 100")
      else new Volume(value)

    implicit object Format extends Format[Volume] {
      override def reads(jv: JsValue): JsResult[Volume] = JsSuccess(Volume(jv.as[Int]))
      override def writes(in: Volume): JsValue = JsNumber(in.value)
    }
  }

  sealed trait SinglePlay

  object SinglePlay {
    case object Yes extends SinglePlay
    case object No extends SinglePlay
    case object OneShot extends SinglePlay

    def apply(s: String): SinglePlay = s match {
      case "0" => No
      case "1" => Yes
      case "oneshot" => OneShot
      case _ => throw new IllegalArgumentException("'" + s + "' is invalid for single.")
    }

    implicit object Format extends Format[SinglePlay] {
      override def reads(jv: JsValue): JsResult[SinglePlay] = JsSuccess(
        SinglePlay(jv.as[String])
      )

      override def writes(in: SinglePlay): JsValue = JsString(
        in match {
          case No => "0"
          case Yes => "1"
          case OneShot => "oneshot"
        }
      )
    }
  }

  sealed trait PlayState

  object PlayState {
    case object Play extends PlayState
    case object Stop extends PlayState
    case object Pause extends PlayState

    def apply(s: String): PlayState = s match {
      case "play" => Play
      case "stop" => Stop
      case "pause" => Pause
      case _ => throw new IllegalArgumentException("'" + s + "' is invalid for play state.")
    }

    implicit object Format extends Format[PlayState] {
      override def reads(jv: JsValue): JsResult[PlayState] = JsSuccess(
        PlayState(jv.as[String])
      )

      override def writes(in: PlayState): JsValue = JsString(
        in match {
          case Play => "play"
          case Stop => "stop"
          case Pause => "pause"
        }
      )
    }
  }

  trait Audio {
    val sampleRate: Int
    val bits: Int
    val channels: Int
  }

  object Audio {
    private case class AudioImpl(sampleRate: Int, bits: Int, channels: Int) extends Audio

    def apply(s: String): Audio = {
      val split = s.split(":")
      AudioImpl(split(0).toInt, split(1).toInt, split(2).toInt)
    }

    implicit object Format extends Format[Audio] {
      override def reads(jv: JsValue): JsResult[Audio] = JsSuccess(
        AudioImpl(
          (jv \ "sampleRate").as[Int],
          (jv \ "bits").as[Int],
          (jv \ "channels").as[Int]
        )
      )

      override def writes(in: Audio): JsValue = Json.obj(
        "sampleRate" -> in.sampleRate,
        "bits" -> in.bits,
        "channels" -> in.channels
      )
    }
  }

  trait StatusInfo {
    val volume: Option[Volume]
    val repeat: Boolean
    val random: Boolean
    val single: SinglePlay
    val consume: Boolean
    val playList: Int
    val playListLength: Int
    val state: PlayState
    val song: Int
    val songId: Int
    val nextSong: Option[Int]
    val nextSongId: Option[Int]
    val elapsed: Double
    val duration: Option[Double]
    val bitRate: Int
    val xfade: Option[Double]
    val mixRampDb: Option[Double]
    val mixRampDelay: Option[Double]
    val audio: Audio
    val updatingDb: Option[Int]
    val error: Option[String]
  }

  object StatusInfo {
    private case class StatusInfoImpl(
      volume: Option[Volume],
      repeat: Boolean,
      random: Boolean,
      single: SinglePlay,
      consume: Boolean,
      playList: Int,
      playListLength: Int,
      state: PlayState,
      song: Int,
      songId: Int,
      nextSong: Option[Int],
      nextSongId: Option[Int],
      elapsed: Double,
      duration: Option[Double],
      bitRate: Int,
      xfade: Option[Double],
      mixRampDb: Option[Double],
      mixRampDelay: Option[Double],
      audio: Audio,
      updatingDb: Option[Int],
      error: Option[String]
    ) extends StatusInfo

    def apply(in: imm.Map[String, String]): StatusInfo = StatusInfoImpl(
      volume = in("volume").toInt match {
        case -1 => None
        case value => Some(Volume(value))
      },
      repeat = in("repeat") == "1",
      random = in("random") == "1",
      single = SinglePlay(in("single")),
      consume = in("consume") == "1",
      playList = in("playlist").toInt,
      playListLength = in("playlistlength").toInt,
      state = PlayState(in("state")),
      song = in("song").toInt,
      songId = in("songid").toInt,
      nextSong = in.get("nextsong").map(_.toInt),
      nextSongId = in.get("nextsongid").map(_.toInt),
      elapsed = in("elapsed").toDouble,
      duration = in.get("duration").map(_.toDouble),
      bitRate = in("bitrate").toInt,
      xfade = in.get("xfade").map(_.toDouble),
      mixRampDb = in.get("maxrampdb").map(_.toDouble),
      mixRampDelay = in.get("maxrampdelay").map(_.toDouble),
      audio = Audio(in("audio")),
      updatingDb = in.get("updatingdb").map(_.toInt),
      error = in.get("error")
    )
  }

  object LsInfo {
    val PlayListPattern = """playlist: (.*)""".r
    val FilePattern = """file: (.*)""".r
    val DirectoryPattern = """directory: (.*)""".r
    val TimePattern = """Time: (.*)""".r
    val LastModifiedPattern = """Last-Modified: (.*)""".r
  }

  private class LsInfoImpl(
    val info: imm.Seq[LsInfoEntry]
  ) extends LsInfo
  
  trait SongInfo {
    val path: String
    val lastModified: Instant
    val length: Int
    val pos: Int
    val id: Int
  }

  object SongInfo {
    private case class SongInfoImpl(
      path: String,
      lastModified: Instant,
      length: Int,
      pos: Int,
      id: Int
    ) extends SongInfo

    def apply(map: imm.Map[String, String]): SongInfo = SongInfoImpl(
      map("file"),
      Instant.parse(map("Last-Modified")),
      map("Time").toInt,
      map("Pos").toInt,
      map("Id").toInt
    )

    implicit object Format extends Format[SongInfo] {
      override def reads(jv: JsValue): JsResult[SongInfo] = JsSuccess(
        SongInfoImpl(
          (jv \ "path").as[String],
          Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong),
          (jv \ "length").as[Int],
          (jv \ "pos").as[Int],
          (jv \ "id").as[Int]
        )
      )

      override def writes(in:SongInfo): JsValue = Json.obj(
        "path" -> in.path,
        "lastModified" -> in.lastModified.toEpochMilli,
        "length" -> in.length,
        "pos" -> in.pos,
        "id" -> in.id
      )
    }
  }

  def batchResult(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalException("Invalid response '" + l + "'")
  }

  def clearError(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalException("Invalid response '" + l + "'")
  }

  def stop(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalException("Invalid response '" + l + "'")
  }

  def add(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def play(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def clear(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def lsInfo(in: BufferedReader): LsInfo = {
    def init(info: imm.Seq[LsInfoEntry] = imm.Seq()): TailRec[LsInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        done(new LsInfoImpl(info))
      case LsInfo.PlayListPattern(path) =>
        tailcall(playList(info, path))
      case LsInfo.FilePattern(path) =>
        tailcall(file(info, path))
      case LsInfo.DirectoryPattern(path) =>
        tailcall(directory(info, path))
      case _ @ l=>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    def playList(info: imm.Seq[LsInfoEntry], path: String): TailRec[LsInfo] = in.readLine match {
      case LsInfo.LastModifiedPattern(time) =>
        tailcall(init(info :+ LsInfoEntry.PlayList(path, Instant.parse(time))))
      case FailPattern(errorNo, commandIdx, command, message) =>
        logger.warn("Play list is truncated path = '" + path + "'")
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        logger.warn("Play list is truncated path = '" + path + "'")
        done(new LsInfoImpl(info))
      case _ @ l=>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    def file(
      info: imm.Seq[LsInfoEntry], path: String,
      lastModified: Option[Instant] = None, length: Option[Int] = None
    ): TailRec[LsInfo] = in.readLine match {
      case LsInfo.LastModifiedPattern(time) =>
        if (length.isDefined)
          tailcall(init(info :+ LsInfoEntry.File(path, Instant.parse(time), length.get)))
        else
          tailcall(file(info, path, Some(Instant.parse(time)), length))
      case LsInfo.TimePattern(length) =>
        if (lastModified.isDefined)
          tailcall(init(info :+ LsInfoEntry.File(path, lastModified.get, length.toInt)))
        else
          tailcall(file(info, path, lastModified, Some(length.toInt)))
      case FailPattern(errorNo, commandIdx, command, message) =>
        logger.warn("File is truncated path = '" + path + "'")
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        logger.warn("File is truncated path = '" + path + "'")
        done(new LsInfoImpl(info))
      case _ @ l=>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    def directory(info: imm.Seq[LsInfoEntry], path: String): TailRec[LsInfo] = in.readLine match {
      case LsInfo.LastModifiedPattern(time) =>
        tailcall(init(info :+ LsInfoEntry.Directory(path, Instant.parse(time))))
      case FailPattern(errorNo, commandIdx, command, message) =>
        logger.warn("Play list is truncated path = '" + path + "'")
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        logger.warn("Play list is truncated path = '" + path + "'")
        done(new LsInfoImpl(info))
      case _ @ l =>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    init().result
  }

  def status(in: BufferedReader): StatusInfo = {
    @tailrec def parse(map: imm.Map[String, String]): StatusInfo = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        StatusInfo(map)
      case _ @ l =>
        val idx = l.indexOf(':')
        if (idx == -1) throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
        else {
          val key = l.substring(0, idx)
          val value = l.substring(idx + 1).trim
          parse(map.updated(key, value))
        }
    }

    parse(imm.Map())
  }

  def currentSong(in: BufferedReader): SongInfo = {
    @tailrec def parse(map: imm.Map[String, String]): SongInfo = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        SongInfo(map)
      case _ @ l =>
        val idx = l.indexOf(':')
        if (idx == -1) throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
        else {
          val key = l.substring(0, idx)
          val value = l.substring(idx + 1).trim
          parse(map.updated(key, value))
        }
    }

    parse(imm.Map())
  }
}
