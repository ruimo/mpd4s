package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait BatchConnection {
  val version: Version

  def clearError(): BatchConnection
  def stop(): BatchConnection
  def clear(): BatchConnection
  def add(path: String): BatchConnection
  def play(idx: Option[Int]): BatchConnection
  def pause(): BatchConnection
  def load(name: String, range: Option[(Int, Int)] = None): BatchConnection
  def deleteId(id: Int): BatchConnection
}
