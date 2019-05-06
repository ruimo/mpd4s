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
    override val args = path.toList
  }
  def lsInfo(path: Option[String]): Request = new LsInfo(path)

  class Add(path: String) extends Command("add") {
    override val args = imm.Seq(path)
  }
  def add(path: String): Request = new Add(path)

  class Play(idx: Option[Int]) extends Command("play") {
    override val args = idx.toList.map(_.toString)
  }
  def play(idx: Option[Int]): Request = new Play(idx)

  class PlayId(id: Int) extends Command("playid") {
    override val args = imm.Seq(id.toString)
  }
  def playId(id: Int): Request = new PlayId(id)

  object pause extends Command("pause")
  object playlistInfo extends Command("playlistinfo")
  object listPlaylists extends Command("listplaylists")
  class Load(name: String, range: Option[(Int, Int)]) extends Command("load") {
    override val args = imm.Seq(name) ++ range.map { case (from, to) => from + ":" + to }
  }
  def load(name: String, range: Option[(Int, Int)]): Request = new Load(name, range)
  class DeleteId(id: Int) extends Command("deleteid") {
    override val args = imm.Seq(id.toString)
  }
  def deleteId(id: Int): Request = new DeleteId(id)
  class MoveId(fromId: Int, toIndex: Int) extends Command("moveid") {
    override val args = imm.Seq(fromId.toString, toIndex.toString)
  }
  def moveId(fromId: Int, toIndex: Int): Request = new MoveId(fromId, toIndex)
  class Save(name: String) extends Command("save") {
    override val args = imm.Seq(name)
  }
  def save(name: String): Request = new Save(name)
  class Rm(name: String) extends Command("rm") {
    override val args = imm.Seq(name)
  }
  def rm(name: String): Request = new Rm(name)
  class Rename(name: String, newName: String) extends Command("rename") {
    override val args = imm.Seq(name, newName)
  }
  def rename(name: String, newName: String) = new Rename(name, newName)
}
