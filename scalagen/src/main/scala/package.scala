package sculptor

import org.typelevel.paiges.Doc

import ast.Package

package object scalagen {

  type Result[A] = Either[String, A]

  def generateDoc(p: Package, c: Config): Result[Doc] =
    run(impl.PackageGen.generate(p: Package), c)

  private[scalagen] def run[A](v: impl.Result[A], c: Config): Result[A] =
    v.value.runA(impl.GeneratorState.init(c)).value
}
