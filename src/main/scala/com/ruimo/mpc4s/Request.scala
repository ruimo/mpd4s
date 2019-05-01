package com.ruimo.mpc4s

import java.io.Writer

import scala.collection.{immutable => imm}

trait Request {
  def toCommand: String

  def args: imm.Seq[String] = imm.Seq()

  def writeln(w: Writer) {
    writeln(w, toCommand)
  }

  private def writeln(w: Writer, l: String) {
    w.write(l)
    if (! args.isEmpty) {
      w.write(" ")
      w.write(args.mkString(" "))
    }
    w.write('\n')
    w.flush()
  }
}

object Request {
  class Command(val toCommand: String) extends Request
  class ClearError extends Command("clearerror")

  class LsInfo(path: Option[String]) extends Command("lsinfo") {
    override def args = path.toList
  }

  val clearError: Request = new ClearError()
  def lsInfo(path: Option[String]): Request = new LsInfo(path)
}
