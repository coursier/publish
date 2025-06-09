package coursier.publish.signing

import coursier.publish.Content
import coursier.publish.fileset.{FileSet, Path}
import coursier.publish.signing.logger.SignerLogger

import java.nio.charset.StandardCharsets
import java.time.Instant

import scala.util.control.NonFatal

/** Signs artifacts.
  */
trait Signer {

  /** Computes the signature of the passed `content`.
    *
    * @return
    *   an error message (left), or the signature file content (right), wrapped in [[Task]]
    */
  def sign(content: Content): Either[String, String]

  /** Adds missing signatures in a [[FileSet]].
    *
    * @param fileSet:
    *   [[FileSet]] to add signatures to - can optionally contain some already calculated signatures
    * @param now:
    *   last modified time for the added signature files
    * @return
    *   a [[FileSet]] of the missing signature files
    */
  def signatures(
    fileSet: FileSet,
    now: Instant,
    dontSignExtensions: Set[String],
    dontSignFiles: Set[String],
    logger: => SignerLogger
  ): Either[(Path, Content, String), FileSet] = {

    val elementsOrSignatures = fileSet.elements.flatMap {
      case (path, content) =>
        if path.elements.lastOption.exists(n => n.endsWith(".asc")) then
          // found a signature
          Seq(Right(path.mapLast(_.stripSuffix(".asc"))))
        else if path.elements.lastOption.exists(n => n.contains(".asc.")) then
          // FIXME May not be ok if e.g. version contains .asc., like 2.0.asc.1 (this is unlikely though)
          // ignored file
          Nil
        else
          // may need to be signed
          Seq(Left((path, content)))
    }

    val signed = elementsOrSignatures
      .collect {
        case Right(path) => path
      }
      .toSet

    val toSign = elementsOrSignatures
      .collect {
        case Left((path, content))
            if !signed(path) &&
            !path.elements.lastOption.exists(n =>
              dontSignExtensions.exists(e => n.endsWith("." + e))
            ) &&
            !path.elements.lastOption.exists(n =>
              dontSignFiles(n) || dontSignFiles.exists(f => n.startsWith(f + "."))
            ) =>
          (path, content)
      }

    def signaturesTask(id: Object, logger0: SignerLogger) =
      toSign.foldLeft[Either[(Path, Content, String), List[(Path, Content)]]](Right(Nil)) {
        case (previous, (path, content)) =>
          previous match {
            case l @ Left(_) => l
            case Right(l)    =>
              def doSign() = sign(content) match {
                case Left(e) =>
                  Left((path, content, e))
                case Right(s) =>
                  val content = Content.InMemory(now, s.getBytes(StandardCharsets.UTF_8))
                  Right((path.mapLast(_ + ".asc"), content) :: l)
              }

              logger0.signingElement(id, path)
              val res =
                try doSign()
                catch {
                  case NonFatal(e) =>
                    logger0.signedElement(id, path, Some(e))
                    throw e
                }
              logger0.signedElement(id, path, None)
              res
          }
      }.map { elements =>
        FileSet(elements.reverse)
      }

    val toSignFs = FileSet(toSign)

    if toSignFs.isEmpty then Right(FileSet.empty)
    else {
      val id      = new Object
      val logger0 = logger
      logger0.start()
      logger0.signing(id, toSignFs)

      try signaturesTask(id, logger0)
      finally {
        logger0.signed(id, toSignFs)
        logger0.stop()
      }
    }
  }
}
