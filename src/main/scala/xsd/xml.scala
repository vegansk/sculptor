package sculptor.xsd

import scala.xml._
import cats.implicits._

object xml {

  def getByNameO(name: String, ns: Option[String] = None)(el: Node): Option[Node] =
    (el \ name)
      .filter(n => ns.map(_ === n.prefix).getOrElse(true))
      .headOption

  def getByName(name: String, ns: Option[String] = None)(el: Node): Either[Throwable, Node] =
    getByNameO(name, ns)(el)
      .toRight(new Exception(s"No such element: ${ns.map(_ + ":" + name).getOrElse(name)} in $el"))

  def findByName(name: String, ns: Option[String] = None)(el: Node): List[Node] =
    (el \ name)
      .filter(n => ns.map(_ === n.prefix).getOrElse(true))
      .toList

  def getAttrO(name: String)(el: Node): Option[String] =
    Option(el \@ name).filter(_.nonEmpty)

  def getAttr(name: String)(el: Node): Either[Throwable, String] =
    getAttrO(name)(el).toRight(new Exception(s"No such attribute: $name"))

  def getPrefix(uri: String)(node: Node): Option[String] =
    Option(node.scope.getPrefix(uri))

  def text(node: Node): String =
    node.text.trim

  type Path = List[Node]

  def mkPath: Path = Nil

  def push(p: Path, n: Node): Path = n :: p

  def pop(p: Path): Path = p.drop(1)
}
