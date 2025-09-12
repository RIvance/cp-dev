package cp.syntax

import cp.core.Environment

trait Synthesis[Target] {
  def synthesize(using env: Environment): Target
}
