package coursier.publish.download.logger

import java.io.PrintStream

final class SimpleDownloadLogger(out: PrintStream, verbosity: Int) extends DownloadLogger {
  override def downloadingIfExists(url: String): Unit = {
    if verbosity >= 2 then out.println(s"Trying to download $url")
  }

  override def downloadedIfExists(
    url: String,
    size: Option[Long],
    errorOpt: Option[Throwable]
  ): Unit =
    if verbosity >= 2 then {
      val msg =
        if size.isEmpty then s"Not found : $url (ignored)"
        else if errorOpt.isEmpty then s"Downloaded $url"
        else s"Failed to download $url"
      out.println(msg)
    }
    else if verbosity >= 1 then
      if size.nonEmpty then out.println(s"Downloaded $url")
}
