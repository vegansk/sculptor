package sculptor

final case class Fix[F[_]](unfix: F[Fix[F]])
final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
