package object recursion { self =>

  import fixpoint.Fix
  import functor._

  type Algebra[F[_], A]   = F[A] => A
  type Coalgebra[F[_], A] = A => F[A]

  // cata: long version
  def cata_0[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A = {
    val unfixed: F[Fix[F]] = fix.unfix
    val fa: F[A]           = unfixed.map(cata_0(algebra))
    val a: A               = algebra(fa)
    a
  }

  // cata: terse version
  def cata[F[_]: Functor, A](algebra: Algebra[F, A])(fix: Fix[F]): A =
    algebra(fix.unfix.map(cata(algebra)))

  final implicit class CataSyntax[F[_]: Functor](private val fix: Fix[F]) {
    @inline def cata[A](algebra: Algebra[F, A]): A =
      self.cata(algebra)(fix)
  }

  // ana: long version
  def ana_0[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] = {
    val fa: F[A]           = coalgebra(a)
    val unfixed: F[Fix[F]] = fa.map(ana_0(coalgebra))
    val fix: Fix[F]        = Fix[F](unfixed)
    fix
  }

  // ana: terse version
  def ana[F[_]: Functor, A](coalgebra: Coalgebra[F, A])(a: A): Fix[F] =
    Fix[F](coalgebra(a).map(ana(coalgebra)))

  final implicit class AnaSyntax[A](private val a: A) {
    @inline def ana[F[_]: Functor](coalgebra: Coalgebra[F, A]): Fix[F] =
      self.ana(coalgebra)(a)
  }
}
