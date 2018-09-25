package sculptor.scalagen

sealed trait Feature

object Feature {

  case object CatsEqTypeclass extends Feature

}

final case class Config(private val tabSize: Option[Int] = Option.empty,
                        private val features: List[Feature] = List.empty)

object Config {
  def tabSize(c: Config): Int = c.tabSize.filter(_ > 0).getOrElse(2)
  def features(c: Config): List[Feature] =
    Map(c.features.map(f => (f.getClass, f)): _*).values.toList
}
