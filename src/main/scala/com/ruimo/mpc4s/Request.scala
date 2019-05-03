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
  object clearError extends Command("clearerror")
  object stop extends Command("stop")
  object clear extends Command("clear")
  object status extends Command("status")
  object currentSong extends Command("currentsong")

  class LsInfo(path: Option[String]) extends Command("lsinfo") {
    override def args = path.toList
  }

  class Add(path: String) extends Command("add") {
    override def args = imm.Seq(path)
  }

  class Play(idx: Option[Int]) extends Command("play") {
    override def args = idx.toList.map(_.toString)
  }

  def lsInfo(path: Option[String]): Request = new LsInfo(path)
  def add(path: String): Request = new Add(path)
  def play(idx: Option[Int]): Request = new Play(idx)
  object pause extends Command("pause")
}
