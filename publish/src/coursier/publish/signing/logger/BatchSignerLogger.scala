package coursier.publish.signing.logger

import coursier.publish.fileset.Path

import java.io.PrintStream

final class BatchSignerLogger(out: PrintStream, verbosity: Int) extends SignerLogger {

  override def signingElement(id: Object, path: Path): Unit =
    if (verbosity >= 0)
      out.println(s"Signing ${path.repr}")
  override def signedElement(id: Object, path: Path, excOpt: Option[Throwable]): Unit =
    if (verbosity >= 0)
      out.println(s"Signed ${path.repr}")
}
