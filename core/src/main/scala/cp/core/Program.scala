package cp.core

class Program(
  val defaultModule: CoreModule,
  val main: Term,
) {
  def terms: Map[String, Term] = defaultModule.terms
  def types: Map[String, Type] = defaultModule.types
}
