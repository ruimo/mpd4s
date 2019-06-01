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
  val UpdatePattern = "updating_db: [0-9]+".r

  def split(l: String): (String, String) = {
    val idx = l.indexOf(':')
    if (idx == -1) throw new IllegalArgumentException("No ':' found in '" + l + "'")
    (l.substring(0, idx), l.substring(idx + 1).trim)
  }

  class ResponseException(
    val errorNo: Int,
    val commandIdx: Int,
    val command: String,
    val message: String
  ) extends Exception(
    "errNo: " + errorNo + ", commandIdx: " + commandIdx + ", command: '" + command + "', message: '" + message + "'"
  )

  class InternalException(msg: String = null, cause: Throwable = null) extends Exception(msg, cause)

  sealed trait LsInfoEntry {
    val path: String
    val lastModified: Instant
  }

  object LsInfoEntry {
    case class Directory(path: String, lastModified: Instant) extends LsInfoEntry
    case class File(path: String, lastModified: Instant, length: Int) extends LsInfoEntry
    case class Playlist(path: String, lastModified: Instant) extends LsInfoEntry

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
          case "playlist" => Playlist(
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
        case Playlist(path, lastModified) => Json.obj(
          "type" -> "playlist",
          "path" -> path,
          "lastModified" -> lastModified.toEpochMilli.toString
        )
      }
    }
  }

  trait LsInfo {
    val entries: imm.Seq[LsInfoEntry]
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
    val playlist: Int
    val playlistLength: Int
    val state: PlayState
    val song: Option[Int]
    val songId: Option[Int]
    val nextSong: Option[Int]
    val nextSongId: Option[Int]
    val elapsed: Option[Double]
    val duration: Option[Double]
    val bitRate: Option[Int]
    val xfade: Option[Double]
    val mixRampDb: Option[Double]
    val mixRampDelay: Option[Double]
    val audio: Option[Audio]
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
      playlist: Int,
      playlistLength: Int,
      state: PlayState,
      song: Option[Int],
      songId: Option[Int],
      nextSong: Option[Int],
      nextSongId: Option[Int],
      elapsed: Option[Double],
      duration: Option[Double],
      bitRate: Option[Int],
      xfade: Option[Double],
      mixRampDb: Option[Double],
      mixRampDelay: Option[Double],
      audio: Option[Audio],
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
      playlist = in("playlist").toInt,
      playlistLength = in("playlistlength").toInt,
      state = PlayState(in("state")),
      song = in.get("song").map(_.toInt),
      songId = in.get("songid").map(_.toInt),
      nextSong = in.get("nextsong").map(_.toInt),
      nextSongId = in.get("nextsongid").map(_.toInt),
      elapsed = in.get("elapsed").map(_.toDouble),
      duration = in.get("duration").map(_.toDouble),
      bitRate = in.get("bitrate").map(_.toInt),
      xfade = in.get("xfade").map(_.toDouble),
      mixRampDb = in.get("mixrampdb").map(_.toDouble),
      mixRampDelay = in.get("mixrampdelay").map(_.toDouble),
      audio = in.get("audio").map(Audio.apply),
      updatingDb = in.get("updatingdb").map(_.toInt),
      error = in.get("error")
    )
  }

  object LsInfo {
    val PlaylistPattern = """playlist: (.*)""".r
    val FilePattern = """file: (.*)""".r
    val DirectoryPattern = """directory: (.*)""".r
    val TimePattern = """Time: (.*)""".r
    val LastModifiedPattern = """Last-Modified: (.*)""".r
  }

  private class LsInfoImpl(
    val entries: imm.Seq[LsInfoEntry]
  ) extends LsInfo
  
  trait PlaylistInfoEntry {
    val path: String
    val lastModified: Instant
    val length: Int
    val pos: Int
    val id: Int
  }

  object PlaylistInfoEntry {
    implicit object Format extends Format[PlaylistInfoEntry] {
      override def reads(jv: JsValue): JsResult[PlaylistInfoEntry] = JsSuccess(
        PlaylistInfoEntryImpl(
          (jv \ "path").as[String],
          Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong),
          (jv \ "length").as[Int],
          (jv \ "pos").as[Int],
          (jv \ "id").as[Int]
        )
      )

      override def writes(in: PlaylistInfoEntry): JsValue = Json.obj(
        "path" -> in.path,
        "lastModified" -> in.lastModified.toEpochMilli.toString,
        "length" -> in.length,
        "pos" -> in.pos,
        "id" -> in.id
      )
    }
  }

  private case class PlaylistInfoEntryImpl(
    path: String, lastModified: Instant, length: Int, pos: Int, id: Int
  ) extends PlaylistInfoEntry

  private object PlaylistInfoEntryImpl {
    def apply(fields: Map[String, String]): PlaylistInfoEntryImpl = PlaylistInfoEntryImpl(
      fields("file"), Instant.parse(fields("Last-Modified")),
      fields("Time").toInt, fields("Pos").toInt, fields("Id").toInt
    )
  }

  trait PlaylistInfo {
    val entries: imm.Seq[PlaylistInfoEntry]
  }

  object PlaylistInfo {
    implicit object Format extends Format[PlaylistInfo] {
      override def reads(jv: JsValue): JsResult[PlaylistInfo] = JsSuccess(
        PlaylistInfoImpl(
          (jv \ "entries").as[Seq[PlaylistInfoEntry]].toList
        )
      )

      override def writes(in: PlaylistInfo): JsValue = Json.obj(
        "entries" -> in.entries
      )
    }
  }

  private case class PlaylistInfoImpl(
    entries: imm.Seq[PlaylistInfoEntry]
  ) extends PlaylistInfo

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
        "lastModified" -> in.lastModified.toEpochMilli.toString,
        "length" -> in.length,
        "pos" -> in.pos,
        "id" -> in.id
      )
    }
  }

  trait StoredPlaylistInfoEntry {
    val name: String
    val lastModified: Instant
  }

  private case class StoredPlaylistInfoEntryImpl(
    name: String, lastModified: Instant
  ) extends StoredPlaylistInfoEntry

  private object StoredPlaylistInfoEntryImpl {
    def apply(fields: Map[String, String]): StoredPlaylistInfoEntryImpl = StoredPlaylistInfoEntryImpl(
      fields("playlist"), Instant.parse(fields("Last-Modified"))
    )
  }

  object StoredPlaylistInfoEntry {
    implicit object Format extends Format[StoredPlaylistInfoEntry] {
      override def reads(jv: JsValue): JsResult[StoredPlaylistInfoEntry] = JsSuccess(
        StoredPlaylistInfoEntryImpl(
          (jv \ "name").as[String],
          Instant.ofEpochMilli((jv \ "lastModified").as[String].toLong)
        )
      )

      override def writes(in: StoredPlaylistInfoEntry): JsValue = Json.obj(
        "name" -> in.name,
        "lastModified" -> in.lastModified.toEpochMilli.toString
      )
    }
  }

  trait StoredPlaylistInfo {
    val entries: imm.Seq[StoredPlaylistInfoEntry]
  }

  private case class StoredPlaylistInfoImpl(
    entries: imm.Seq[StoredPlaylistInfoEntry]
  ) extends StoredPlaylistInfo

  object StoredPlaylistInfo {
    implicit object Format extends Format[StoredPlaylistInfo] {
      override def reads(jv: JsValue): JsResult[StoredPlaylistInfo] = JsSuccess(
        StoredPlaylistInfoImpl(
          (jv \ "entries").as[Seq[StoredPlaylistInfoEntry]].toList
        )
      )

      override def writes(in: StoredPlaylistInfo): JsValue = Json.obj(
        "entries" -> in.entries
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

  def pause(in: BufferedReader): Unit = in.readLine match {
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
      case LsInfo.PlaylistPattern(path) =>
        tailcall(playlist(info, path))
      case LsInfo.FilePattern(path) =>
        tailcall(file(info, path))
      case LsInfo.DirectoryPattern(path) =>
        tailcall(directory(info, path))
      case _ @ l=>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    def playlist(info: imm.Seq[LsInfoEntry], path: String): TailRec[LsInfo] = in.readLine match {
      case LsInfo.LastModifiedPattern(time) =>
        tailcall(init(info :+ LsInfoEntry.Playlist(path, Instant.parse(time))))
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

  def currentSong(in: BufferedReader): Option[SongInfo] = {
    @tailrec def parse(map: imm.Map[String, String]): Option[SongInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        if (map.isEmpty) None else Some(SongInfo(map))
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

  def playlistInfo(in: BufferedReader): PlaylistInfo = {
    def init(
      entries: imm.Seq[PlaylistInfoEntry] = imm.Seq()
    ): TailRec[PlaylistInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        done(PlaylistInfoImpl(entries))
      case l =>
        val (key, value) = split(l)
        tailcall(start(entries, Map(key -> value)))
    }

    def start(
      entries: imm.Seq[PlaylistInfoEntry], fields: Map[String, String]
    ): TailRec[PlaylistInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        done(PlaylistInfoImpl(entries :+ PlaylistInfoEntryImpl(fields)))
      case l =>
        val (key, value) = split(l)
        if (key == "file")
          tailcall(start(entries :+ PlaylistInfoEntryImpl(fields), Map(key -> value)))
        else
          tailcall(start(entries, fields.updated(key, value)))
    }

    init().result
  }

  def listPlaylists(in: BufferedReader): StoredPlaylistInfo = {
    def init(
      entries: imm.Seq[StoredPlaylistInfoEntry] = imm.Seq()
    ): TailRec[StoredPlaylistInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        done(StoredPlaylistInfoImpl(entries))
      case l =>
        val (key, value) = split(l)
        tailcall(start(entries, Map(key -> value)))
    }

    def start(
      entries: imm.Seq[StoredPlaylistInfoEntry], fields: Map[String, String]
    ): TailRec[StoredPlaylistInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
      case OkPattern() =>
        done(StoredPlaylistInfoImpl(entries :+ StoredPlaylistInfoEntryImpl(fields)))
      case l =>
        val (key, value) = split(l)
        if (key == "playlist")
          tailcall(start(entries :+ StoredPlaylistInfoEntryImpl(fields), Map(key -> value)))
        else
          tailcall(start(entries, fields.updated(key, value)))
    }

    init().result
  }

  def load(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def deleteId(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def moveId(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def save(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def rm(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def rename(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def playId(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case OkPattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }

  def update(in: BufferedReader): Unit = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      throw new ResponseException(errorNo.toInt, commandIdx.toInt, command, message)
    case UpdatePattern() =>
    case _ @ l =>
      throw new InternalError(new IllegalArgumentException("Invalid response '" + l + "'"))
  }
}
