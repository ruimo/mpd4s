package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait Connection {
  def clearError(): Unit
  def stop(): Unit
  def clear(): Unit
  def lsInfo(path: Option[String]): Response.LsInfo
  def add(path: String): Unit
  def play(idx: Option[Int]): Unit
  def status(): Response.StatusInfo
  def currentSong(): Option[Response.SongInfo]
}
