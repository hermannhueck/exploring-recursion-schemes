package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix

/*
  To resolve the recursive type structure problem, we use fixpoint:

  final case class Fix[F[_]](unfix: F[Fix[F]])

  Fix is a case class with a type constructor 'F' which wraps a value (called 'unfix')
  of type F[Fix[F]] .

  Our values 'calc1' and 'calc2' now get a simpler type which hides the recursion.
  But the nesteing of values is duplicated, as every CalcF is wrapped in a Fix:

  before:
    val calc2: CalcF[CalcF[CalcF[Nothing]]] =
      MulF(NumF(3), AddF(NumF(1), NumF(2)))

  after:
    val calc2: Fix[CalcF] =
      Fix(MulF(
          Fix(NumF[Fix[CalcF]](3)),
          Fix(AddF(
              Fix(NumF[Fix[CalcF]](1)),
              Fix(NumF[Fix[CalcF]](2))
            ))
        ))

  Fix[CalcF] is a unified type for a possibly deeply nested structure. To get
  access to the CalcF's we just have to 'unfix' it.

  eval and show now take a Fix[CalcF] as parameter instead of a CalcF[???].
  These functions must 'unfix' the wrapped Fix[CalcF] values before pattern matching
  over the different cases. The functions can be used with any depth of nesting
  of CalcF's.

  We can print/process our structure again which was not possible after step 2.
 */
object Cata03Calc extends util.App {

  sealed trait CalcF[+A] extends Product with Serializable
  object CalcF {
    case class NumF[A](i: Int)     extends CalcF[A]
    case class AddF[A](a: A, b: A) extends CalcF[A]
    case class MulF[A](a: A, b: A) extends CalcF[A]
  }

  import CalcF.{AddF, MulF, NumF}

  // FORMAT: OFF
  // 1 + 2
  val calc1: Fix[CalcF] =
    Fix(AddF(
        Fix(NumF[Fix[CalcF]](1)),
        Fix(NumF[Fix[CalcF]](2))
      )).tap(println)

  // 3 * (1 + 2)
  val calc2: Fix[CalcF] =
    Fix(MulF(
        Fix(NumF[Fix[CalcF]](3)),
        Fix(AddF(
            Fix(NumF[Fix[CalcF]](1)),
            Fix(NumF[Fix[CalcF]](2))
          ))
      )).tap(println)
  // FORMAT: ON

  val eval: Fix[CalcF] => Int =
    _.unfix match {
      case NumF(i)    => i
      case AddF(a, b) => eval(a) + eval(b)
      case MulF(a, b) => eval(a) * eval(b)
    }

  val show: Fix[CalcF] => String =
    _.unfix match {
      case NumF(i)    => i.toString
      case AddF(a, b) => s"(${show(a)} + ${show(b)})"
      case MulF(a, b) => s"${show(a)} * ${show(b)}"
    }

  println
  show(calc1) pipe (str => println(s"show: $str"))
  eval(calc1) pipe (res => println(s"eval: $res"))
  println
  show(calc2) pipe (str => println(s"show: $str"))
  eval(calc2) pipe (res => println(s"eval: $res"))
}
