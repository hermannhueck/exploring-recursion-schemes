package my

package object cataimpl {

  import Calc._
  import CalcF._

  def calc2CalcF[A]: Calc => CalcF[A] = {

    final implicit class CalcSyntax(c: CalcF[A]) {
      def asA: A = c.asInstanceOf[A]
    }

    _ match {
      case Num(i)    => NumF(i)
      case Add(a, b) => AddF[A](calc2CalcF(a).asA, calc2CalcF(b).asA)
      case Mul(a, b) => MulF(calc2CalcF(a).asA, calc2CalcF(b).asA)
    }
  }

  final implicit class CalcSyntax(calc: Calc) {
    def toCalcF[A]: CalcF[A] = calc2CalcF(calc)
  }

  def calcF2Calc[A]: CalcF[A] => Calc = {

    final implicit class CalcFSyntax(a: A) {
      def asCalcF = a.asInstanceOf[CalcF[A]]
    }

    _ match {
      case NumF(i)    => Num(i)
      case AddF(a, b) => Add(calcF2Calc(a.asCalcF), calcF2Calc(b.asCalcF))
      case MulF(a, b) => Mul(calcF2Calc(a.asCalcF), calcF2Calc(b.asCalcF))
    }
  }

  final implicit class CalcFSyntax[A](calcF: CalcF[A]) {
    def toCalc: Calc = calcF2Calc(calcF)
  }
}
