package scalau.text

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.Spec


class VersionSpec extends Spec with ShouldMatchers {

  private val testString = "3.1.0.666"
  private val testList = List(3, 1, 0, 666)

  describe("A version") {

    it("should create a string representation") {
      Version(testList: _*) should equal (testString)
    }

    it("should split a string representation") {
      val Version(first, ver@_*) = testString
      ver.toList should equal (testList.tail)
      first should equal (testList.head)
    }

    it("should fail to split an invalid string") {
      val test = "3.x.55"
      test match {
        case Version(ver) => error("should not have matched")
        case x => x should equal (test)
      }
    }

  }
}
