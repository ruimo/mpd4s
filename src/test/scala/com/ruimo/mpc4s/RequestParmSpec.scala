package com.ruimo.mpc4s

import org.specs2.mutable.Specification

class RequestParmSpec extends Specification {
  "Request parm" should {
    "Can treat string" in {
      import com.ruimo.mpc4s.RequestParam._

      val req = RequestParam("Hello")
      req.toStringParam === "Hello"
    }

    "Can treat escape" in {
      import com.ruimo.mpc4s.RequestParam._

      val req = RequestParam("Hello World")
      req.toStringParam === """"Hello World""""
    }

    "Can treat quote" in {
      import com.ruimo.mpc4s.RequestParam._

      val req = RequestParam("Hello Ruimo's room")
      req.toStringParam === """"Hello Ruimo\'s room""""
    }

    "Can treat int" in {
      import com.ruimo.mpc4s.RequestParam._

      val req = RequestParam(123)
      req.toStringParam === "123"
    }
  }
}
