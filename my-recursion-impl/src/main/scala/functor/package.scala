package object functor {

  final implicit class FunctorSyntax[F[_]: Functor, A](private val fa: F[A]) {
    @inline def map[B](f: A => B): F[B] =
      Functor[F].map(fa)(f)
  }
}
