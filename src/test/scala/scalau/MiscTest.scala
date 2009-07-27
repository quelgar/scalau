package scalau

import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers


final class MiscTest extends Spec with ShouldMatchers {

  import Misc._

  describe("Nullable to option (?)") {

    it("should convert null to None") {
      ?(null) should be (None)
    }

    it("should convert non-null to Some") {
      ?(666) should be (Some(666))
      val test = "testing"
      ?(test) should be (Some(test))
    }

  }

  describe("Null dereference block (??) aka 'Elvis'") {

    final class Test(val id: Int, val inner: Test)

    val start = new Test(1, new Test(2, null))

    it("should convert chain without null to Some") {
      ??(start.id) should be (Some(1))
      ??(start.inner.id) should be (Some(2))
    }

    it("should convert intermediate null to None") {
      ??(start.inner.inner.id) should be (None)
      ??(start.inner.inner.inner.id) should be (None)
    }

    it("should convert null expression to None") {
      ??(start.inner.inner) should be (None)
    }
  }

}
