package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait Connection {
  def clearError(): Unit
  def stop(): Unit
  def lsInfo(path: Option[String]): Response.LsInfo
  def status(): Response.StatusInfo
  def currentSong(): Option[Response.SongInfo]
}
