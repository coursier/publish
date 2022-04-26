package coursier.publish.sonatype

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import coursier.core.Authentication
import coursier.util.Task
import sttp.client3._
import sttp.model.{Header, Uri}

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters._
import scala.util.Try

private[sonatype] final case class HttpClientUtil(
  backend: SttpBackend[Identity, Any],
  authentication: Option[Authentication],
  verbosity: Int
) {

  private def request(url: String, post: Option[Array[Byte]] = None, isJson: Boolean = false) = {

    val uri = Uri.parse(url) match {
      case Left(err)   => ???
      case Right(uri0) => uri0
    }
    val authHeaders = authentication
      .toSeq
      .flatMap(_.allHttpHeaders)
      .map {
        case (k, v) => new Header(k, v)
      }

    val contentTypeHeaders =
      if (isJson)
        Seq(new Header("Content-Type", "application/json"))
      else
        Nil

    val req = basicRequest
      .header(
        "Accept",
        "application/json,application/vnd.siesta-error-v1+json,application/vnd.siesta-validation-errors-v1+json"
      )
      .headers(authHeaders ++ contentTypeHeaders: _*)
      .response(asByteArrayAlways)

    post match {
      case Some(body) =>
        req
          .body(post.getOrElse(Array.emptyByteArray))
          .post(uri)
      case None =>
        req.get(uri)
    }
  }

  def create(url: String, post: Option[Array[Byte]] = None, isJson: Boolean = false): Unit = {

    if (verbosity >= 1)
      Console.err.println(s"Getting $url")
    val resp = request(url, post, isJson).send(backend)
    if (verbosity >= 1)
      Console.err.println(s"Done: $url")

    if (resp.code.code != 201)
      throw new Exception(
        s"Failed to get $url (http status: ${resp.code.code}, response: ${Try(new String(resp.body, StandardCharsets.UTF_8)).getOrElse("")})"
      )
  }

  def get[T: JsonValueCodec](
    url: String,
    post: Option[Array[Byte]] = None,
    nested: Boolean = true,
    isJson: Boolean = false
  ): T = {

    if (verbosity >= 1)
      Console.err.println(s"Getting $url")
    val resp = request(url, post, isJson).send(backend)
    if (verbosity >= 1)
      Console.err.println(s"Done: $url")

    if (resp.code.isSuccess)
      if (nested)
        try readFromArray[T](resp.body)
        catch {
          case e: JsonReaderException =>
            throw new Exception(s"Received invalid response from $url", e)
        }
      else
        try readFromArray[T](resp.body)
        catch {
          case e: JsonReaderException =>
            throw new Exception(s"Received invalid response from $url", e)
        }
    else {
      val msg =
        s"Failed to get $url (http status: ${resp.code.code}, response: ${Try(new String(resp.body, StandardCharsets.UTF_8)).getOrElse("")})"
      if (resp.code.isClientError)
        throw new FileNotFoundException(msg)
      else
        throw new Exception(msg)
    }
  }
}
