package object recursion {

  import fixpoint.Fix
  import functor._

  type Algebra[F[_], A]   = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fixedData: Fix[F]): A =
    algebra(fixedData.unfix.map(cata(algebra)))

  final implicit class FixSyntax[F[_]: Functor](private val fix: Fix[F]) {
    @inline def cata[A](algebra: Algebra[F, A]): A =
      recursion.cata(algebra)(fix)
  }

  def ana[F[_]: Functor, A](coalg: Coalgebra[F, A])(a: A): Fix[F] = {
    ???
  }
}
