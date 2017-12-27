package sculptor
package tsgen

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
