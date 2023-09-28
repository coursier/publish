package coursier.publish.util

import scala.math.Numeric.Implicits

final case class EmaRetryParams(
  attempts: Int,
  initialWaitDurationMs: Long,
  factor: Float
) {
  def update(value: Long): Long =
    math.ceil(value * factor.toDouble).toLong
}
