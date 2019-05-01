package com.ruimo.mpc4s

import java.io.BufferedReader

sealed trait FinalResponse

trait Response {
  def finalResponse: FinalResponse
}

object Response {
  val OkPattern = "OK".r
  val FailPattern = """ACK \[([0-9]+)@(.*)\] \{(.*)\} (.*)""".r

  case object Ok extends FinalResponse
  case class Fail(errorNo: Int, commandIdx: Int, command: String, message: String) extends FinalResponse

  trait ClearError extends Response

  private class SimpleResponse(val finalResponse: FinalResponse)

  private class ClearErrorImpl(finalResponse: FinalResponse) extends SimpleResponse(finalResponse) with ClearError

  def clearError(in: BufferedReader): ClearError = in.readLine match {
    case FailPattern(errorNo, commandIdx, command, message) =>
      new ClearErrorImpl(Fail(errorNo.toInt, commandIdx.toInt, command, message))
    case OkPattern() =>
      new ClearErrorImpl(Ok)
    case _ @ l =>
      throw new IllegalArgumentException("Invalid response '" + l + "'")
  }
}


