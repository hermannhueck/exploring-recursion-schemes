package freecofree.recursion

sealed trait Free[F[_], A]                           extends Product with Serializable
final case class Continue[F[_], A](a: A)             extends Free[F, A]
final case class Combine[F[_], A](fa: F[Free[F, A]]) extends Free[F, A]

object Free {
  def continue[F[_], A](a: A): Free[F, A]             = Continue(a)
  def combine[F[_], A](fa: F[Free[F, A]]): Free[F, A] = Combine(fa)
}
