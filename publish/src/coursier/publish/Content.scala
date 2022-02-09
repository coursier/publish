package coursier.publish

import java.nio.file.{Files, Path}
import java.time.Instant

/** Content of a file, either on disk or in memory.
  */
sealed abstract class Content extends Product with Serializable {
  def lastModified(): Instant
  // TODO Support chunked reading
  def content(): Array[Byte]

  def pathOpt: Option[Path] = None
}

object Content {

  final case class File(path: Path) extends Content {
    def lastModified(): Instant =
      Files.getLastModifiedTime(path).toInstant
    def content(): Array[Byte] =
      Files.readAllBytes(path)
    override def pathOpt: Option[Path] =
      Some(path)
  }

  final case class InMemory(lastModified: Instant, content0: Array[Byte]) extends Content {
    def content(): Array[Byte] =
      content0
  }

}
