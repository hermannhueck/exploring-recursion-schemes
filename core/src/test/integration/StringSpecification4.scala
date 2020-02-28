/*
  Integrating ScalaCheck into ScalaTest (PropSpec style)
  - we replace the Assertions with ShouldMatchers
  - this version doesn't use ScalaCheck at all, but ScalaCheck must be in the claspath.
 */

package integration

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class StringSpecification4 extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {

  property("startsWith") {
    forAll { (a: String, b: String) =>
      (a + b) should startWith(a)
    }
  }

  property("concatenate") {
    forAll { (a: String, b: String) => // failing test
      (a + b).length should be > a.length
      (a + b).length should be > b.length
    }
  }

  property("substring") {
    forAll { (a: String, b: String, c: String) =>
      (a + b + c).substring(a.length, a.length + b.length) shouldBe b
    }
  }
}
