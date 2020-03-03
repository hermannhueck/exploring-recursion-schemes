package my
package fixpoint

final case class Fix[F[_]](unfix: F[Fix[F]])
