package sculptor
package scalagen

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
