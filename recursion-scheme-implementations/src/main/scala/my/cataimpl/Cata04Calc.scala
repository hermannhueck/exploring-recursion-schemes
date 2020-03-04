package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor.Functor

/*
  In step 3 we still had recursive calls in eval and show:

    val eval: Fix[CalcF] => Int =
      _.unfix match {
        case NumF(i)    => i
        case AddF(a, b) => eval(a) + eval(b)
        case MulF(a, b) => eval(a) * eval(b)
      }

  show is implemented accordingly, but it produces a String instead of an Int.

  Here in step 4 we want to factor out recursion from eval and show.
  We simplify eval and show by passing an unfixed value of type CalcF[A]
  where A is the return type of the respective function, Int for eval,
  String for show:

    val eval: CalcF[Int] => Int = {
      case NumF(i)    => i
      case AddF(a, b) => a + b
      case MulF(a, b) => a * b
    }

  We also removed the recursion out of eval and show. We just invoke Int operations
  (+ and *) in eval and String operations in show.

  signature of eval:                      CalcF[Int] => Int
  signature of show:                      CalcF[String] => String
  abstracting over the result type:       CalcF[A] => A
  abstracting over the structure type:    F[A] => A

  unfix and recursion now must be done outside of eval and show.

  Recursion can be factored out be means of a Functor, a so called Pattern Functor:

  We could use Functor from cats or from Scalaz. Here we use our own Functor implementation
  in package my.functor.

  The Functor is defined as implicit instance in the CalcF companion object:

    implicit val functor: Functor[CalcF] = new Functor[CalcF] {
      override def map[A, B](fa: CalcF[A])(f: A => B): CalcF[B] =
        fa map f
    }

  The map implementaion in the Functor instance delegates to the map method in the
  CalcF trait which applies the mapping function f in the recursive cases AddF and MulF:

  sealed trait CalcF[+A] extends Product with Serializable {
    def map[B](f: A => B): CalcF[B] =
      this match {
        case NumF(i)    => NumF(i)
        case AddF(a, b) => AddF(f(a), f(b))
        case MulF(a, b) => MulF(f(a), f(b))
      }
  }

  unfix and recursive call have been removed from eval and show. But these must
  now happen somewhere else.

  We do this in our function collapseCalcFToA which takes a function CalcF[A] => A and a Fix[CalcF]
  and returns an A. The function also requires an implicit Functor[CalcF]. This function
  collapses a recursive structure Fix[CalcF] to a single value of type A.

    def collapseCalcFToA[A](calc2Value: CalcF[A] => A)(fixedCalc: Fix[CalcF])(implicit calcFunctor: Functor[CalcF]): A = {
      val unfixed: CalcF[Fix[CalcF]] = fixedCalc.unfix
      val recursed: CalcF[A]         = unfixed.map(collapseCalcFToA(calc2Value))
      val a: A                       = calc2Value(recursed)
      a
    }

  The implementation is separated in three steps:
  - unfix the Fix[CalcF] structure
  - recurse generically using the pattern functor for CalcF which results in a single A value wrapped in a CalcF.
  - a final application of the mapping function CalcF[A] => A turns the recursion result into an A.
    The value of type A is returned.

  We have abstracted unfix recursion from eval and show and implemented it in a generic way
  which works for all A's which have a Pattern Functor that maps CalcF[A] => A.

  In step 5 we abstract over CalcF and make our collapse function work for any F[_]
  that has a Pattern Functor instance.

  To simplify the invocation of 'map' for CalcF we defined the implicit class FunctorSyntax:

    final implicit class FunctorSyntax[A](private val calc: CalcF[A]) {
      def map[B](f: A => B): CalcF[B] =
        Functor[CalcF].map(calc)(f)
    }
 */
object Cata04Calc extends util.App {

  sealed trait CalcF[+A] extends Product with Serializable {

    import CalcF.{AddF, MulF, NumF}

    def map[B](f: A => B): CalcF[B] =
      this match {
        case NumF(i)    => NumF(i)
        case AddF(a, b) => AddF(f(a), f(b))
        case MulF(a, b) => MulF(f(a), f(b))
      }
  }

  object CalcF {

    case class NumF[A](i: Int)     extends CalcF[A]
    case class AddF[A](a: A, b: A) extends CalcF[A]
    case class MulF[A](a: A, b: A) extends CalcF[A]

    implicit val functor: Functor[CalcF] = new Functor[CalcF] {
      override def map[A, B](fa: CalcF[A])(f: A => B): CalcF[B] =
        fa map f
    }
  }

  import CalcF.{AddF, MulF, NumF}

  final implicit class FunctorSyntax[A](private val calc: CalcF[A]) {
    @inline def map[B](f: A => B): CalcF[B] =
      Functor[CalcF].map(calc)(f)
  }

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

  val eval: CalcF[Int] => Int = {
    case NumF(i)    => i
    case AddF(a, b) => a + b
    case MulF(a, b) => a * b
  }

  val show: CalcF[String] => String = {
    case NumF(i)    => i.toString
    case AddF(a, b) => s"($a + $b)"
    case MulF(a, b) => s"$a * $b"
  }

  def collapseCalcFToA[A](calc2Value: CalcF[A] => A)(fixedCalc: Fix[CalcF])(implicit calcFunctor: Functor[CalcF]): A = {
    val unfixed: CalcF[Fix[CalcF]] = fixedCalc.unfix
    val recursed: CalcF[A]         = unfixed.map(collapseCalcFToA(calc2Value))
    val a: A                       = calc2Value(recursed)
    a
  }

  println
  collapseCalcFToA(show)(calc1) pipe (str => println(s"show: $str"))
  collapseCalcFToA(eval)(calc1) pipe (res => println(s"eval: $res"))
  println
  collapseCalcFToA(show)(calc2) pipe (str => println(s"show: $str"))
  collapseCalcFToA(eval)(calc2) pipe (res => println(s"eval: $res"))
}
