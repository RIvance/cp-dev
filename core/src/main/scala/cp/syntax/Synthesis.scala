package cp.syntax

import cp.core.{Type, Term}

trait Synthesis[Target] {
  def synthesize: Target
}
