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
