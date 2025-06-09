package coursier.publish.download

import coursier.core.Authentication
import coursier.publish.download.logger.DownloadLogger
import sttp.client3.*
import sttp.model.{Header, Uri}

import java.time.Instant
import java.util.Date

import scala.util.control.NonFatal

final case class HttpDownload(
  backend: SttpBackend[Identity, Any]
) extends Download {
  def downloadIfExists(
    url: String,
    authentication: Option[Authentication],
    logger: DownloadLogger
  ): Option[(Option[Instant], Array[Byte])] = {

    val uri = Uri.parse(url) match {
      case Left(_)     => ???
      case Right(uri0) => uri0
    }

    val authHeaders = authentication
      .toSeq
      .flatMap(_.allHttpHeaders)
      .map {
        case (k, v) => new Header(k, v)
      }

    val req = basicRequest
      .headers(authHeaders*)
      .response(asByteArrayAlways)
      .get(uri)
    logger.downloadingIfExists(url)

    val res =
      try {
        val resp = req.send(backend)

        if (resp.isSuccess) {
          val lastModifiedOpt = resp.header("Last-Modified").map { value =>
            Instant.ofEpochMilli(Date.parse(value))
          }
          Some((lastModifiedOpt, resp.body))
        }
        else if (resp.isClientError)
          None
        else
          ???
      }
      catch {
        case NonFatal(e) =>
          logger.downloadedIfExists(
            url,
            None,
            Some(new Download.Error.DownloadError(url, e))
          )
          throw e
      }

    logger.downloadedIfExists(
      url,
      res.map(_._2.length),
      None
    )

    res
  }
}
