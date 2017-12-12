package sculptor
package iots

package object generator {

  def create(config: Config): Generator = new Generator(config)

}
