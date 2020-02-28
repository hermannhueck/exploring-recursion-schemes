/*
  Integrating ScalaCheck into ScalaTest (FlatSpec style)
  - changes StringSpecification4 to use FlatSpec style
 */

package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class StringSpecification5 extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  "Two concatenated strings" should "start with the first string" in {
    forAll { (a: String, b: String) =>
      (a + b) should startWith(a)
    }
  }

  "Two concatenated strings" should "have a length >= the length of the first or second string" in {
    forAll { (a: String, b: String) => // failing test
      (a + b).length should be > a.length
      (a + b).length should be > b.length
    }
  }

  "Three concatenated strings" should "have the second string as a substring in the middle" in {
    forAll { (a: String, b: String, c: String) =>
      (a + b + c).substring(a.length, a.length + b.length) shouldBe b
    }
  }
}
