package coursier.publish.upload

import coursier.cache.CacheUrl
import coursier.core.Authentication
import coursier.publish.upload.logger.UploadLogger
import sttp.client3.*
import sttp.model.{Header, Uri}

import java.io.InputStream

final case class HttpUpload(
  backend: SttpBackend[Identity, Any],
  expect100Continue: Boolean
) extends Upload {
  def upload(
    url: String,
    authentication: Option[Authentication],
    content: Array[Byte],
    logger: UploadLogger,
    loggingIdOpt: Option[Object]
  ): Option[Upload.Error] = {
    val maybeUri = Uri.parse(url).left.map { _ =>
      ???
    }

    val bodyInputStream: InputStream =
      new InputStream {
        private var pos                    = 0
        private def addToPos(n: Int): Unit = {
          pos += n
          logger.progress(url, loggingIdOpt, pos, content.length)
        }
        override def read(): Int =
          if pos < content.length then {
            val value = content(pos)
            addToPos(1)
            value
          }
          else -1
        override def available(): Int                              = content.length - pos
        override def read(b: Array[Byte], off: Int, len: Int): Int =
          if pos < content.length then {
            val toRead = len.min(available())
            System.arraycopy(content, pos, b, off, toRead)
            addToPos(toRead)
            toRead
          }
          else -1
        override def readNBytes(b: Array[Byte], off: Int, len: Int): Int =
          if pos < content.length then {
            val toRead = len.min(available())
            System.arraycopy(content, pos, b, off, toRead)
            addToPos(toRead)
            toRead
          }
          else 0
        override def skip(n: Long): Long =
          if pos < content.length then {
            val toSkip = n.min(available())
            addToPos(toSkip.toInt)
            toSkip
          }
          else 0
      }

    val expect100Headers =
      if expect100Continue then Seq(new Header("Expect", "100-continue")) else Nil

    val authHeaders = authentication
      .toSeq
      .flatMap(_.allHttpHeaders)
      .map {
        case (k, v) => new Header(k, v)
      }

    val e = maybeUri.flatMap { uri =>
      val req = basicRequest
        .body(bodyInputStream)
        .headers(expect100Headers ++ authHeaders*)
        .put(uri)
      logger.uploading(url, loggingIdOpt, Some(content.length))
      val resp = req.send(backend)
      if resp.isSuccess then Right(())
      else if resp.code.code == 401 then
        Left {
          val realmOpt = resp.header("WWW-Authenticate").collect {
            case CacheUrl.BasicRealm(r) => r
          }
          new Upload.Error.Unauthorized(url, realmOpt)
        }
      else
        Left {
          new Upload.Error.HttpError(
            resp.code.code,
            resp.headers.map(h => (h.name, h.value)).groupBy(_._1).iterator.map { case (k, l) =>
              (k, l.map(_._2))
            }.toMap,
            resp.body.merge
          )
        }
    }

    e.left.toOption
  }
}
