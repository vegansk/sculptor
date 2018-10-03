package sculptor.tsgen
package impl

final case class GeneratorState(config: Config)

object GeneratorState {
  def init(c: Config): GeneratorState = GeneratorState(c)
}
