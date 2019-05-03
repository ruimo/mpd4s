package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait BatchConnection {
  def clearError(): BatchConnection
  def stop(): BatchConnection
  def clear(): BatchConnection
  def add(path: String): BatchConnection
  def play(idx: Option[Int]): BatchConnection
}
