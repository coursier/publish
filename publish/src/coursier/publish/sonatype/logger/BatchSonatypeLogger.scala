package coursier.publish.sonatype.logger

import java.io.PrintStream

final class BatchSonatypeLogger(out: PrintStream, verbosity: Int) extends SonatypeLogger {
  override def listingProfiles(attempt: Int, total: Int): Unit =
    if verbosity >= 0 then {
      val extra = if attempt == 0 then "" else s" (attempt $attempt / $total)"
      out.println("Listing Sonatype profiles..." + extra)
    }
  override def listedProfiles(errorOpt: Option[Throwable]): Unit = {

    val msgOpt =
      if errorOpt.isEmpty then
        if verbosity >= 1 then Some("Listed Sonatype profiles")
        else None
      else Some("Fail to list Sonatype profiles")

    for (msg <- msgOpt)
      out.println(s"$msg")
  }
}
