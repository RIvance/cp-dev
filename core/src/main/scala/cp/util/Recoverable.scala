package cp.util

import cp.error.{CoreError, CpError}

import scala.util.{Failure, Success, Try}

object Recoverable {
  /** Constructs a `Try` using the by-name parameter as a result value.
   *
   * The evaluation of `computation` is attempted once.
   *
   * Any `CpError` exception is caught and results in a Failure`
   * that holds the exception.
   *
   * @param computation the result value to compute
   * @return the result of evaluating the value, as a `Success` or `Failure`
   */
  def apply[T](computation: => T): Try[T] = {
    try {
      val result = computation
      Success(result)
    } catch {
      case e: CpError => Failure(e)
    }
  }
}
