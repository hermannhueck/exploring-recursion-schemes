/*
  Integrating ScalaCheck into ScalaTest (FlatSpec style)
  - uses labels and a ScalaCheck generator
 */

package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.matchers.should.Matchers

class StringSpecification6 extends AnyFlatSpec with ScalaCheckPropertyChecks with Matchers {

  "Two concatenated strings" should "start with the first string" in {

    import org.scalacheck.Arbitrary
    import org.scalacheck.Gen
    val gen: Gen[String] = Arbitrary.arbitrary[String]

    forAll(gen -> "first", gen -> "second") { (a: String, b: String) =>
      (a + b) should startWith(a)
    }
  }

  "Two concatenated strings" should "have a length >= the length of the first or second string" in {
    forAll("first", "second") { (a: String, b: String) => // failing test
      (a + b).length should be > a.length
      (a + b).length should be > b.length
    }
  }

  "Three concatenated strings" should "have the second string as a substring in the middle" in {
    forAll("first", "second", "third") { (a: String, b: String, c: String) =>
      (a + b + c).substring(a.length, a.length + b.length) shouldBe b
    }
  }
}
