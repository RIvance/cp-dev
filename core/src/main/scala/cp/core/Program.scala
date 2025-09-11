package cp.core

class Program(
  val defaultModule: Module,
  val main: Term,
) {
  def terms: Map[String, Term] = defaultModule.terms
  def types: Map[String, Type] = defaultModule.types
}
