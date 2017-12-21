package sculptor
package scala

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
