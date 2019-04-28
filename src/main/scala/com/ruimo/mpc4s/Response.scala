package com.ruimo.mpc4s

sealed trait Response

object Response {
  case class Ok(text: String)
  case class Fail(errorNo: Int, commandIdx: Int, command: String, message: String)
}
