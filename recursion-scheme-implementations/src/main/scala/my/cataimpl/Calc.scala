package my
package cataimpl

sealed trait Calc extends Product with Serializable

object Calc {
  case class Num(i: Int)           extends Calc
  case class Add(a: Calc, b: Calc) extends Calc
  case class Mul(a: Calc, b: Calc) extends Calc
}
