package com.ruimo.mpc4s

import java.net.InetAddress
import java.net.Socket

trait Connection {
  def clearError(): Response.ClearError
}
