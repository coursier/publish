package coursier.publish.sonatype.logger

import coursier.cache.internal.Terminal.Ansi

import java.io.{OutputStream, OutputStreamWriter}

final class InteractiveSonatypeLogger(out: OutputStreamWriter, verbosity: Int)
    extends SonatypeLogger {
  override def listingProfiles(attempt: Int, total: Int): Unit =
    if verbosity >= 0 then {
      val extra = if attempt == 0 then "" else s" (attempt $attempt / $total)"
      out.write("Listing Sonatype profiles..." + extra)
      out.flush()
    }
  override def listedProfiles(errorOpt: Option[Throwable]): Unit = {
    if verbosity >= 0 then {
      out.clearLine(2)
      out.write('\n')
      out.up(1)
      out.flush()
    }

    val msgOpt =
      if errorOpt.isEmpty then
        if verbosity >= 1 then Some("Listed Sonatype profiles") else None
      else Some("Fail to list Sonatype profiles")

    for (msg <- msgOpt) {
      out.write(s"$msg" + System.lineSeparator())
      out.flush()
    }
  }
}

object InteractiveSonatypeLogger {
  def create(out: OutputStream, verbosity: Int): SonatypeLogger =
    new InteractiveSonatypeLogger(new OutputStreamWriter(out), verbosity)
}
