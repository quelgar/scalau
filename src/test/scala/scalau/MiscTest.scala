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

}
