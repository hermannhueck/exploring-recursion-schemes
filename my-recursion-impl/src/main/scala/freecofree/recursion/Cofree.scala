package freecofree
package recursion

final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
