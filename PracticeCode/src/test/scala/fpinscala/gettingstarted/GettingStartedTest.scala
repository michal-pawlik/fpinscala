package fpinscala.gettingstarted

import org.scalatest.WordSpec

import scala.fpinscala.gettingstarted.GettingStarted

class GettingStartedTest extends WordSpec {

    ".printHello" when {
      "return Hello" in {
        assertResult("Hello"){
          GettingStarted.printHello
        }
      }
    }

}
