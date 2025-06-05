package coursier.publish.util

final case class EmaRetryParams(
  attempts: Int,
  initialWaitDurationMs: Long,
  factor: Float
) {
  def update(value: Long): Long =
    math.ceil(value * factor.toDouble).toLong
}
