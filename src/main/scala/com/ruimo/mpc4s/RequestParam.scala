package com.ruimo.mpc4s

import scala.annotation.tailrec

sealed trait RequestParam[+T] {
  def toStringParam: String
}

object RequestParam {
  import scala.language.implicitConversions

  def apply[T](value: T)(implicit f: T => RequestParam[T]): RequestParam[T] = f.apply(value)

  implicit def fromString(s: String): RequestParam[String] = {
    @tailrec def escape(needQuote: Boolean = false, buf: StringBuilder = new StringBuilder, i: Int = 0): String =
      if (i >= s.length)
        if (needQuote) '"' + buf.toString + '"' else buf.toString
      else {
        val c = s.charAt(i)
        if (c == ' ') {
          escape(true, buf.append(c), i + 1)
        } else if (c == '\\' || c == '"' || c == '\'') {
          buf.append('\\')
          escape(true, buf.append(c), i + 1)
        } else {
          escape(needQuote, buf.append(c), i + 1)
        }
      }

    new RequestParam[String] {
      val toStringParam = escape()
    }
  }

  implicit def fromInt(i: Int): RequestParam[Int] = new RequestParam[Int] {
    val toStringParam = i.toString
  }
}
