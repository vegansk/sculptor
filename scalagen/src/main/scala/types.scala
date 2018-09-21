package sculptor.scalagen

final case class Config(tabSize: Option[Int] = None)

object Config {
  def tabSize(c: Config): Int = c.tabSize.filter(_ > 0).getOrElse(2)
}
