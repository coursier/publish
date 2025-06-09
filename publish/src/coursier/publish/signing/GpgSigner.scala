package coursier.publish.signing

import coursier.publish.Content

import java.nio.charset.StandardCharsets
import java.nio.file.attribute.{PosixFilePermission, PosixFilePermissions}
import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*

final case class GpgSigner(
  key: GpgSigner.Key,
  command: String = "gpg",
  extraOptions: Seq[String] = Nil
) extends Signer {

  private def keyArgs: Seq[String] =
    key match {
      case GpgSigner.Key.Default =>
        Nil
      case GpgSigner.Key.Id(id) =>
        Seq("--local-user", id)
    }

  def sign(content: Content): Either[String, String] = {

    val (path, temporary) = content.pathOpt.map(p => (p, false)).getOrElse {
      val p = Files.createTempFile(
        "signer",
        ".content",
        PosixFilePermissions.asFileAttribute(
          Set(
            PosixFilePermission.OWNER_READ,
            PosixFilePermission.OWNER_WRITE
          ).asJava
        )
      )
      val b = content.content()
      Files.write(p, b)
      (p, true)
    }

    sign0(path, temporary, content)
  }

  private def sign0(
    path: Path,
    temporary: Boolean,
    content: Content
  ): Either[String, String] = {
    // inspired by https://github.com/jodersky/sbt-gpg/blob/853e608120eac830068bbb121b486b7cf06fc4b9/src/main/scala/Gpg.scala
    val dest = Files.createTempFile(
      "signer",
      ".asc",
      PosixFilePermissions.asFileAttribute(
        Set(
          PosixFilePermission.OWNER_READ,
          PosixFilePermission.OWNER_WRITE
        ).asJava
      )
    )

    try {
      val pb = new ProcessBuilder()
        .command(
          Seq(command) ++
            extraOptions ++
            keyArgs ++
            Seq(
              "--armor",
              "--yes",
              "--output",
              dest.toAbsolutePath.toString,
              "--detach-sign",
              path.toAbsolutePath.toString
            )*
        )
        .inheritIO()

      val p = pb.start()

      val retCode = p.waitFor()

      if retCode == 0 then Right(new String(Files.readAllBytes(dest), StandardCharsets.UTF_8))
      else Left(s"gpg failed (return code: $retCode)")
    }
    finally {
      // Ignore I/O errors?
      Files.deleteIfExists(dest)
      if temporary then Files.deleteIfExists(path)
    }
  }
}

object GpgSigner {
  sealed abstract class Key extends Product with Serializable

  object Key {
    final case class Id(id: String) extends Key
    case object Default             extends Key
  }
}
