/*
  Integrating ScalaCheck into ScalaTest (PropSpec style)
  - define test as a class instead of an object
  - the "property" method now comes form "PropSpec"
  - mixin Checkers to provide the "check" method
  - check processes the Prop returned from ScalaChecks Prop.forAll
 */

package integration

import org.scalacheck.Prop.forAll
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.Checkers

class StringSpecification2 extends AnyPropSpec with Checkers {

  property("startsWith") {
    check {
      forAll { (a: String, b: String) =>
        (a + b).startsWith(a)
      }
    }
  }

  property("concatenate") {
    check {
      forAll { (a: String, b: String) =>
        (a + b).length > a.length && (a + b).length > b.length // failing test
      }
    }
  }

  property("substring") {
    check {
      forAll { (a: String, b: String, c: String) =>
        (a + b + c).substring(a.length, a.length + b.length) == b
      }
    }
  }
}
