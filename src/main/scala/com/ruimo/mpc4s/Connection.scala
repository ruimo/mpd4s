package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait Connection {
  val version: Version

  def clearError(): Unit
  def stop(): Unit
  def clear(): Unit
  def lsInfo(path: Option[String]): Response.LsInfo
  def add(path: String): Unit
  def play(idx: Option[Int]): Unit
  def status(): Response.StatusInfo
  def currentSong(): Option[Response.SongInfo]
  def pause(): Unit
  def playlistInfo(): Response.PlaylistInfo
  def listPlaylists(): Response.StoredPlaylistInfo
  def load(name: String, range: Option[(Int, Int)] = None): Unit
  def deleteId(id: Int): Unit
  def moveId(fromId: Int, toIndex: Int): Unit
  def save(name: String): Unit
  def rm(name: String): Unit
  def rename(name: String, newName: String): Unit
  def playId(id: Int): Unit
  def update(uri: String): Unit
}
