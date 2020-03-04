package my.cataimpl

import scala.util.chaining._

/*
  In this little tutorial I want to derive the catamorphism 'cata',
  a generic function to fold any higher-kinded rercursive structure.

  Let's start with a simple ADT 'Calc' defined in Calc.scala:

  sealed trait Calc extends Product with Serializable

  object Calc {
    case class Num(i: Int)           extends Calc
    case class Add(a: Calc, b: Calc) extends Calc
    case class Mul(a: Calc, b: Calc) extends Calc
  }

  Num encapsulates an Int value,
  Add and Mul encapsulate two other Calc values to be added or multiplied.

  We can construct finite recursive structures: 'calc1' and 'calc2',
  but their type is not recursive, it's just 'Calc'.

  If we want to process such a nested structure we have to pattern match
  recursively over it.

  That is what 'eval' and 'show' do. In case it is a 'Num' these functions
  just process the encapsulated value. In case it is an 'Add' or a 'Mul',
  'eval' and 'show' recursively call themselves.

  Recursion is in the algorithem but not in the type. This is impossible
  as long as Calc is not a type constructor.
 */
object Cata01Calc extends util.App {

  import Calc._

  // 1 + 2
  val calc1: Calc =
    Add(Num(1), Num(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2: Calc =
    Mul(Num(3), Add(Num(1), Num(2)))
      .tap(println)

  val eval: Calc => Int =
    _ match {
      case Num(i)    => i
      case Add(a, b) => eval(a) + eval(b)
      case Mul(a, b) => eval(a) * eval(b)
    }

  def show: Calc => String =
    _ match {
      case Num(i)    => i.toString
      case Add(a, b) => s"(${show(a)} + ${show(b)})"
      case Mul(a, b) => s"${show(a)} * ${show(b)}"
    }

  println
  calc1 pipe show pipe println
  calc1 pipe eval pipe println
  println
  calc2 pipe show pipe println
  calc2 pipe eval pipe println
}
