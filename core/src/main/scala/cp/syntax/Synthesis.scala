package cp.syntax

import cp.util.SourceSpan

trait Synthesis[Target] {
  def synthesize: Target
}
