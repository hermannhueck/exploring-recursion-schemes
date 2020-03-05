package my
package cataimpl

import fixpoint._
import functor._

sealed trait CalcF[+A] extends Product with Serializable {

  import CalcF._

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

  implicit val functorCalcF: Functor[CalcF] = new Functor[CalcF] {
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

  import Calc._

  final implicit class CalcSyntax(calc: Calc) {
    def toCalcF: CalcF[Calc] = calc2CalcF(calc)
  }

  import recursion._

  // Algebras: F[A] => A

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

  // another Algebra turns a CalcF[Calc] back to a Calc
  val calcF2Calc: Algebra[CalcF, Calc] = {
    case NumF(i)    => Num(i)
    case AddF(a, b) => Add(a, b)
    case MulF(a, b) => Mul(a, b)
  }

  // Coalgebras: A => F[A]

  val calc2CalcF: Coalgebra[CalcF, Calc] = {
    case Num(i)    => NumF(i)
    case Add(a, b) => AddF(a, b)
    case Mul(a, b) => MulF(a, b)
  }
}
