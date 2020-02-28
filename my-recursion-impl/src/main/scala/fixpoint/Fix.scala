package fixpoint

case class Fix[F[_]](unfix: F[Fix[F]])
