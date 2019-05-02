package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait Connection {
  def clearError(): Response.ClearError
  def stop(): Response.Stop
  def lsInfo(path: Option[String]): Response.LsInfo
}
