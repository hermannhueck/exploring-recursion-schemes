/*
  Integrating ScalaCheck into ScalaTest (PropSpec style)
  - define test as a class instead of an object
  - the "property" method now comes form "PropSpec"
  - ScalaCheckPropertyChecks gives us forAll
  - we don't need ScalaCheck's Prop.forAll any more
  - but now we must return Assertions instead of Booleans
 */

package integration

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringSpecification3 extends AnyPropSpec with ScalaCheckPropertyChecks {

  property("startsWith") {
    forAll { (a: String, b: String) =>
      assert((a + b).startsWith(a))
    }
  }

  property("concatenate") {
    forAll { (a: String, b: String) =>
      assert((a + b).length > a.length && (a + b).length > b.length) // failing test
    }
  }

  property("substring") {
    forAll { (a: String, b: String, c: String) =>
      assert((a + b + c).substring(a.length, a.length + b.length) == b)
    }
  }
}
