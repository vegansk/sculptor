package sculptor.scalagen

sealed trait Feature

object Feature {

  case object CatsEqTypeclass extends Feature

}

final case class Config(private val tabSize: Int = 2,
                        private val features: List[Feature] = List.empty,
                        private val lineWidth: Int = 120)

object Config {
  def tabSize(c: Config): Int = c.tabSize
  def features(c: Config): List[Feature] =
    Map(c.features.map(f => (f.getClass, f)): _*).values.toList
  def lineWidth(c: Config): Int = c.lineWidth
}
