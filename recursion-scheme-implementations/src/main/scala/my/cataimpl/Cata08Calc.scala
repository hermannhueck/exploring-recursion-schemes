package my
package cataimpl

import scala.util.chaining._
import fixpoint.Fix
import functor._

/*
  Step 8 condenses the cata impl to a one-liner.

  Additionally we provide 'fix' in the CalcF trait which delegates to the CalcF companion object.

  CalcF.fix turns a CalcF[A] into a Fix[CalcF]. It creates a recursive Fix structure for any CalcF[A].

    def fix[A]: CalcF[A] => Fix[CalcF] = {
      case NumF(i) =>
        Fix(NumF[Fix[CalcF]](i)) // needs type param for type inference
      case AddF(a, b) =>
        Fix(AddF(fixA(a), fixA(b)))
      case MulF(a, b) =>
        Fix(MulF(fixA(a), fixA(b)))
    }

  This simplifies the creation of Fix structure of any depth.
  We can create calc1 and calc2 as simply as in step 2 and apply 'fix' afterwards

  before:

    val calc2: Fix[CalcF] =
      Fix(MulF(
          Fix(NumF[Fix[CalcF]](3)),
          Fix(AddF(
              Fix(NumF[Fix[CalcF]](1)),
              Fix(NumF[Fix[CalcF]](2))
            ))
        ))

  after:

    val calc2 =
      MulF(NumF(3), AddF(NumF(1), NumF(2)))

    val fixed: Fix[CalcF] = fix(calc2) // or alternatively:
    val fixed2: Fix[CalcF] = calc2.fix
 */
object Cata08Calc extends util.App {

  sealed trait CalcF[+A] extends Product with Serializable {

    import CalcF.{AddF, MulF, NumF}

    def map[B](f: A => B): CalcF[B] =
      this match {
        case NumF(i)    => NumF(i)
        case AddF(a, b) => AddF(f(a), f(b))
        case MulF(a, b) => MulF(f(a), f(b))
      }

    def fix: Fix[CalcF] = CalcF.fix(this)
  }

  object CalcF {

    case class NumF[A](i: Int)     extends CalcF[A]
    case class AddF[A](a: A, b: A) extends CalcF[A]
    case class MulF[A](a: A, b: A) extends CalcF[A]

    implicit val functor: Functor[CalcF] = new Functor[CalcF] {
      override def map[A, B](fa: CalcF[A])(f: A => B): CalcF[B] =
        fa map f
    }

    private def fixA[A](a: A): Fix[CalcF] =
      fix apply a.asInstanceOf[CalcF[A]]

    def fix[A]: CalcF[A] => Fix[CalcF] = {
      case NumF(i) =>
        Fix(NumF[Fix[CalcF]](i)) // needs type param for type inference
      case AddF(a, b) =>
        Fix(AddF(fixA(a), fixA(b)))
      case MulF(a, b) =>
        Fix(MulF(fixA(a), fixA(b)))
    }
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

  type Algebra[F[_], A]   = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  val eval: Algebra[CalcF, Int] = {
    case NumF(i)    => i
    case AddF(a, b) => a + b
    case MulF(a, b) => a * b
  }

  val show: Algebra[CalcF, String] = {
    case NumF(i)    => i.toString
    case AddF(a, b) => s"($a + $b)"
    case MulF(a, b) => s"$a * $b"
  }

  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fixedData: Fix[F]): A =
    algebra(fixedData.unfix.map(cata(algebra)))

  println
  cata(show)(CalcF.fix(calc1)) pipe (str => println(s"show: $str"))
  cata(eval)(CalcF.fix(calc1)) pipe (res => println(s"eval: $res"))
  println
  cata(show)(calc2.fix) pipe (str => println(s"show: $str"))
  cata(eval)(calc2.fix) pipe (res => println(s"eval: $res"))
}
