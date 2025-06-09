package coursier.publish.dir

import coursier.publish.Content
import coursier.publish.dir.logger.DirLogger
import coursier.publish.fileset.{FileSet, Path as FsPath}

import java.nio.file.{Files, Path}

import scala.collection.compat.immutable.LazyList
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

object Dir {

  def fileSet(dir: Path, logger: DirLogger): FileSet = {

    def files(f: Path): LazyList[Path] =
      if Files.isRegularFile(f) then {
        logger.element(dir, f)
        LazyList(f)
      }
      else if Files.isDirectory(f) then {
        var s: java.util.stream.Stream[Path] = null
        try {
          s = Files.list(f)
          s.iterator()
            .asScala
            .toVector
            .to(LazyList)
            .flatMap(files)
        }
        finally if s != null then s.close()
      }
      else
        // ???
        LazyList.empty

    val dir0  = dir.normalize().toAbsolutePath
    val elems = files(dir0).toVector.map { f =>
      val p       = FsPath(dir0.relativize(f).iterator().asScala.map(_.toString).toVector)
      val content = Content.File(f)
      (p, content)
    }
    FileSet(elems)
  }

  def isRepository(dir: Path): Boolean = {

    def isMetadata(f: Path): Boolean = {
      val name = f.getFileName.toString
      name.endsWith(".pom") || name.endsWith(".xml")
    }

    // Some(false) if this directory or any of its sub-directories:
    //   - contains files,
    //   - but none of them looks like metadata (*.pom or *.xml)
    // Some(true) if this directory or any of its sub-directories:
    //   - contains files,
    //   - and all that do have files that look like metadata (*.pom or *.xml)
    // None else (no files found, only directories).
    def validate(f: Path): Option[Boolean] = {

      val (dirs, files) = {
        var s: java.util.stream.Stream[Path] = null
        try {
          s = Files.list(f)
          s.iterator().asScala.toVector.partition(Files.isDirectory(_))
        }
        finally
          if s != null then s.close()
      }

      val checkFiles =
        if files.isEmpty then None
        else Some(files.exists(isMetadata))

      // there should be a monoid for that…

      checkFiles match {
        case Some(false) => checkFiles
        case _           =>
          val checkDirs =
            dirs.foldLeft(Option.empty[Boolean]) {
              (acc, dir) =>
                acc match {
                  case Some(false) => acc
                  case _           =>
                    validate(dir) match {
                      case r @ Some(_) => r
                      case None        => acc
                    }
                }
            }

          checkDirs match {
            case Some(_) => checkDirs
            case None    => checkFiles
          }
      }
    }

    Files.isDirectory(dir) &&
    validate(dir).getOrElse(false)
  }

  def read(dir: Path, logger: => DirLogger): FileSet = {

    val logger0 = logger
    logger0.start()
    logger0.reading(dir)

    try {
      val res =
        try fileSet(dir, logger0)
        catch {
          case NonFatal(e) =>
            logger0.read(dir, 0)
            throw e
        }
      logger0.read(dir, res.elements.length)
      res
    }
    finally logger0.stop()
  }

}
