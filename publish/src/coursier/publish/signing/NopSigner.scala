package coursier.publish.signing

import java.time.Instant

import coursier.publish.Content
import coursier.publish.fileset.{FileSet, Path}
import coursier.publish.signing.logger.SignerLogger
import coursier.util.Task

object NopSigner extends Signer {
  def sign(content: Content): Either[String, String] =
    Right("")

  override def signatures(
    fileSet: FileSet,
    now: Instant,
    dontSignExtensions: Set[String],
    dontSignFiles: Set[String],
    logger: => SignerLogger
  ): Either[(Path, Content, String), FileSet] =
    Right(FileSet.empty)
}
