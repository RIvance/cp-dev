package cp.intermediate.calculus.compiler

import cp.core.{Environment, Module as CpModule, Term as CpTerm, Type as CpType}
import cp.intermediate.calculus.RcTerm
import cp.intermediate.TypeValue as RcType

class RcCompiler(module: CpModule) {

  private type Env = Environment[Int, RcType, RcTerm]

  extension (term: CpTerm) {
    def compile(using env: Env): RcTerm = this.compileTerm(term, env)
  }

  def compileTerm(term: CpTerm, env: Env): RcTerm = ???
}
