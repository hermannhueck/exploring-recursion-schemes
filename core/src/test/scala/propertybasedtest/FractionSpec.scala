package propertybasedtest

import org.scalatest._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks

class FractionSpec
    extends flatspec.AnyFlatSpec
    with matchers.should.Matchers
    with ScalaCheckPropertyChecks
    with TableDrivenPropertyChecks {

  // Generator-driven property check
  "A Fraction" should "should be normalized (denominator always > 0)" in {

    forAll { (n: Int, d: Int) =>
      whenever(d != 0 && d != Integer.MIN_VALUE && n != Integer.MIN_VALUE) {

        val f = new Fraction(n, d)

        if (n < 0 && d < 0 || n > 0 && d > 0)
          f.numer should be > 0
        else if (n != 0)
          f.numer should be < 0
        else
          f.numer should ===(0)

        f.denom should be > 0
      }
    }
  }

  // Table-driven property check
  it should "throw an IllegalArgumentException for invalid combinations of d and n" in {

    val invalidCombos =
      Table(
        ("n", "d"),
        (Integer.MIN_VALUE, Integer.MIN_VALUE),
        (1, Integer.MIN_VALUE),
        (Integer.MIN_VALUE, 1),
        (Integer.MIN_VALUE, 0),
        (1, 0)
      )

    forAll(invalidCombos) { (n: Int, d: Int) =>
      an[IllegalArgumentException] should be thrownBy {
        new Fraction(n, d)
      }
    }
  }
}
