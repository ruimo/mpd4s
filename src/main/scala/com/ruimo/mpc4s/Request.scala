package com.ruimo.mpc4s

import java.io.Writer
import Response.ClearError

trait Request[+R] {
  def toCommand: String

  def writeln(w: Writer) {
    writeln(w, toCommand)
  }

  private def writeln(w: Writer, l: String) {
    w.write(l)
    w.write('\n')
    w.flush()
  }
}

object Request {
  private case class NoArgCommand[+R](val toCommand: String) extends Request[R]

  val clearError: Request[ClearError] = NoArgCommand[ClearError]("clearerror")
}
