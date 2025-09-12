package cp.syntax

import cp.core.Environment
import cp.util.SourceSpan

trait Synthesis[Target] {
  def synthesize(using env: Environment = Environment.empty): Target
}
