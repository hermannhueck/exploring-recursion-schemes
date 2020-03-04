package my.cataimpl

import scala.util.chaining._

/*
  In step 2 we make 'Calc' a higher-kinded type called 'CalcF'.
  We give it a (covariant) type parameter A. The types nested
  in Add and Mul are no longer other Calc's, they become just A's:

  before:     case class Add    (a: Calc, b: Calc) extends Calc
  after:      case class AddF[A](a: A,    b: A   ) extends CalcF[A]

  We do that in order to define a Functor on CalcF later, a so called Pattern Functor.
  This is not possible for Calc, as Calc is not higher-kinded.

  The construction of a structure of CalcF[???] seames a bit weird at the
  first glance. Now we have the recursion in the type of the structure
  we constructeed. See calc1 and calc2.

  The implementations of eval and and show don't change.
  But the signatures changed:

  before:     val eval: Calc           => Int
  after:      val eval: CalcF[Nothing] => Int

  eval is now a function from CalcF[Nothing] => Int. Nothing is a good fit
  for the type parameter as it is the bottom type, the sub type of all types.

  But now we are stuck. We cannot use eval and show, as these functions
  expect a value of type CalcF[Nothing], but our calc1 and calc2 have a
  recursive type:
  val calc2: CalcF[CalcF[CalcF[Nothing]]] = ???

  To solve this problem we should have written an eval and show for every
  supported depth of nesting of CalcF's. But this solution wouldn't scale.

  A generic solution for any depth of nesting is given in step 3 with Fix.
 */
object Cata02Calc extends util.App {

  sealed trait CalcF[+A] extends Product with Serializable
  object CalcF {
    case class NumF[A](i: Int)     extends CalcF[A]
    case class AddF[A](a: A, b: A) extends CalcF[A]
    case class MulF[A](a: A, b: A) extends CalcF[A]
  }

  import CalcF.{AddF, MulF, NumF}

  // 1 + 2
  val calc1: CalcF[CalcF[Nothing]] =
    AddF(NumF(1), NumF(2))
      .tap(println)

  // 3 * (1 + 2)
  val calc2: CalcF[CalcF[CalcF[Nothing]]] =
    MulF(NumF(3), AddF(NumF(1), NumF(2)))
      .tap(println)

  val eval: CalcF[Nothing] => Int =
    _ match {
      case NumF(i)    => i
      case AddF(a, b) => eval(a) + eval(b)
      case MulF(a, b) => eval(a) * eval(b)
    }

  val show: CalcF[Nothing] => String =
    _ match {
      case NumF(i)    => i.toString
      case AddF(a, b) => s"(${show(a)} + ${show(b)})"
      case MulF(a, b) => s"${show(a)} * ${show(b)}"
    }

  // eval(calc1)
  // [error] [E1] recursion-scheme-implementations/src/main/scala/my/cataimpl/Cata02Calc.scala
  // [error]      type mismatch;
  // [error]       found   : my.cataimpl.Cata02Calc.CalcF[my.cataimpl.Cata02Calc.CalcF[Nothing]]
  // [error]       required: my.cataimpl.Cata02Calc.CalcF[Nothing]
  // [error]      L66:   eval(calc1)
  // [error]                  ^
}
