package sculptor
package tsgen
package deprecated

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
