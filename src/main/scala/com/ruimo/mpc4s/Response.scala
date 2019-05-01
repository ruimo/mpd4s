package com.ruimo.mpc4s

import java.io.BufferedReader
import java.time.Instant
import scala.collection.{immutable => imm}
import scala.annotation.tailrec
import scala.util.control.TailCalls.TailRec
import scala.util.control.TailCalls._
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

sealed trait FinalResponse

trait Response {
  def finalResponse: FinalResponse
}

sealed trait LsInfoEntry {
  val path: String
  val lastModified: Instant
}

object LsInfoEntry {
  case class Directory(path: String, lastModified: Instant) extends LsInfoEntry
  case class File(path: String, lastModified: Instant, length: Int) extends LsInfoEntry
  case class PlayList(path: String, lastModified: Instant) extends LsInfoEntry
}

object Response {
  val logger = LoggerFactory.getLogger(getClass)

  val OkPattern = "OK".r
  val FailPattern = """ACK \[([0-9]+)@(.*)\] \{(.*)\} (.*)""".r

  case object Ok extends FinalResponse
  case class Fail(errorNo: Int, commandIdx: Int, command: String, message: String) extends FinalResponse

  trait ClearError extends Response
  trait LsInfo extends Response {
    val info: imm.Seq[LsInfoEntry]
  }
  object LsInfo {
    val PlayListPattern = """playlist: (.*)""".r
    val FilePattern = """file: (.*)""".r
    val DirectoryPattern = """directory: (.*)""".r
    val TimePattern = """Time: (.*)""".r
    val LastModifiedPattern = """Last-Modified: (.*)""".r
  }

  private class ResponseBase(val finalResponse: FinalResponse)

  private class ClearErrorImpl(finalResponse: FinalResponse) extends ResponseBase(finalResponse) with ClearError
  private class LsInfoImpl(
    finalResponse: FinalResponse,
    val info: imm.Seq[LsInfoEntry]
  ) extends ResponseBase(finalResponse) with LsInfo

  def clearError(in: BufferedReader): ClearError = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      new ClearErrorImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message))
    case OkPattern() =>
      new ClearErrorImpl(Ok)
    case _ @ l =>
      throw new IllegalArgumentException("Invalid response '" + l + "'")
  }

  def lsInfo(in: BufferedReader): LsInfo = {
    def init(info: imm.Seq[LsInfoEntry] = imm.Seq()): TailRec[LsInfo] = in.readLine match {
      case FailPattern(errorNo, commandIdx, command, message) =>
        done(new LsInfoImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message), info))
      case OkPattern() =>
        done(new LsInfoImpl(Ok, info))
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
        done(new LsInfoImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message), info))
      case OkPattern() =>
        logger.warn("Play list is truncated path = '" + path + "'")
        done(new LsInfoImpl(Ok, info))
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
        done(new LsInfoImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message), info))
      case OkPattern() =>
        logger.warn("File is truncated path = '" + path + "'")
        done(new LsInfoImpl(Ok, info))
      case _ @ l=>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    def directory(info: imm.Seq[LsInfoEntry], path: String): TailRec[LsInfo] = in.readLine match {
      case LsInfo.LastModifiedPattern(time) =>
        tailcall(init(info :+ LsInfoEntry.Directory(path, Instant.parse(time))))
      case FailPattern(errorNo, commandIdx, command, message) =>
        logger.warn("Play list is truncated path = '" + path + "'")
        done(new LsInfoImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message), info))
      case OkPattern() =>
        logger.warn("Play list is truncated path = '" + path + "'")
        done(new LsInfoImpl(Ok, info))
      case _ @ l =>
        logger.warn("Unknown lsinfo entry '" + l + "'")
        tailcall(init(info))
    }

    init().result
  }
}
