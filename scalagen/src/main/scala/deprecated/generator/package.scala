package sculptor.scalagen.deprecated

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
