package coursier.publish.checksum

import coursier.publish.Content
import coursier.publish.checksum.logger.ChecksumLogger
import coursier.publish.fileset.FileSet
import coursier.util.Task

import java.nio.charset.StandardCharsets
import java.time.Instant
import java.util.concurrent.ExecutorService

import scala.util.control.NonFatal

object Checksums {

  def clear(types: Seq[ChecksumType], fs: FileSet): FileSet = {
    val extensions = types.map("." + _.extension)
    FileSet(
      fs.elements.filter {
        case (p, _) =>
          p.elements
            .lastOption
            .forall(n => !extensions.exists(n.endsWith))
      }
    )
  }

  /** Compute the missing checksums in a [[FileSet]].
    *
    * @param types:
    *   checksum types to check / compute
    * @param fileSet:
    *   initial [[FileSet]], can optionally contain some already calculated checksum
    * @param now:
    *   last modified time for the added checksum files
    * @return
    *   a [[FileSet]] of the missing checksum files
    */
  def apply(
    types: Seq[ChecksumType],
    fileSet: FileSet,
    now: Instant,
    pool: ExecutorService,
    logger: => ChecksumLogger
  ): Task[FileSet] = {

    // separate base files from existing checksums
    val filesOrChecksums = fileSet
      .elements
      .map {
        case (path, content) =>
          types
            .collectFirst {
              case t if path.elements.lastOption.exists(_.endsWith("." + t.extension)) =>
                (path.mapLast(_.stripSuffix("." + t.extension)), t)
            }
            .toLeft((path, content))
      }

    val checksums = filesOrChecksums
      .collect {
        case Left(e) => e
      }
      .toSet

    val files = filesOrChecksums
      .collect {
        case Right(p) => p
      }

    val missing =
      for {
        type0           <- types
        (path, content) <- files
        if !checksums((path, type0))
      } yield (type0, path, content)

    // compute missing checksum files
    def checksumFilesTask(id: Object, logger0: ChecksumLogger) =
      Task.gather.gather {
        missing.map {
          case (type0, path, content) =>
            val checksumPath = path.mapLast(_ + "." + type0.extension)
            Task.schedule(pool) {
              logger0.computing(id, type0, checksumPath.repr)
              val res =
                try {
                  val b        = content.content()
                  val checksum = Checksum.compute(type0, b)
                  (
                    checksumPath,
                    Content.InMemory(now, checksum.repr.getBytes(StandardCharsets.UTF_8))
                  )
                }
                catch {
                  case NonFatal(e) =>
                    logger0.computed(id, type0, checksumPath.repr, Some(e))
                    throw e
                }
              logger0.computed(id, type0, checksumPath.repr, None)
              res
            }
        }
      }
        .map { elements =>
          FileSet(elements)
        }

    val missingFs = FileSet(
      missing.map {
        case (type0, path, content) =>
          val checksumPath = path.mapLast(_ + "." + type0.extension)
          (checksumPath, content) // kind of meh… content isn't used here anyway
      }
    )

    val before = Task.delay {
      val id      = new Object
      val logger0 = logger
      logger0.start()
      logger0.computingSet(id, missingFs)
      (id, logger0)
    }

    def after(id: Object, logger0: ChecksumLogger) = Task.delay {
      logger0.computedSet(id, missingFs)
      logger0.stop()
    }

    for {
      idLogger <- before
      (id, logger0) = idLogger
      a   <- checksumFilesTask(id, logger0).attempt
      _   <- after(id, logger0)
      res <- Task.fromEither(a)
    } yield res
  }

}
