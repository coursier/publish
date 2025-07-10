package coursier.publish.sonatype

import com.github.plokhotnyuk.jsoniter_scala.core.*
import coursier.core.Authentication
import sttp.client3.*
import sttp.model.{Header, Uri}

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets

import scala.jdk.CollectionConverters.*
import scala.util.Try

private[sonatype] final case class HttpClientUtil(
  backend: SttpBackend[Identity, Any],
  authentication: Option[Authentication],
  verbosity: Int
) {

  private def request(url: String, post: Option[Array[Byte]] = None, isJson: Boolean = false) = {
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

    val contentTypeHeaders =
      if isJson then Seq(new Header("Content-Type", "application/json")) else Nil

    val allHeaders = authHeaders ++ contentTypeHeaders
    val req        = basicRequest
      .header(
        "Accept",
        "application/json,application/vnd.siesta-error-v1+json,application/vnd.siesta-validation-errors-v1+json"
      )
      .headers(allHeaders*)
      .response(asByteArrayAlways)

    val result =
      post match {
        case Some(_) =>
          if verbosity >= 2 then Console.err.println(s"POST request to $uri")
          req
            .body(post.getOrElse(Array.emptyByteArray))
            .post(uri)
        case None =>
          if verbosity >= 2 then Console.err.println(s"GET request to $uri")
          req.get(uri)
      }
    if verbosity >= 2 then Console.err.println(s"Request created successfully: $uri")
    result
  }

  def create(url: String, post: Option[Array[Byte]] = None, isJson: Boolean = false): Unit = {
    val resp = createResponse(url, post, isJson)
    if resp.code.code != 201 then
      throw new Exception(
        s"Failed to get $url (http status: ${resp.code.code}, response: ${Try(new String(resp.body, StandardCharsets.UTF_8)).getOrElse("")})"
      )
  }

  def createResponse(
    url: String,
    post: Option[Array[Byte]] = None,
    isJson: Boolean = false
  ): Response[Array[Byte]] = {
    if verbosity >= 1 then Console.err.println(s"Getting $url")
    val resp = request(url, post, isJson).send(backend)
    if verbosity >= 1 then Console.err.println(s"Got HTTP ${resp.code.code} from $url")
    resp
  }

  def getEmptyResponse(
    url: String,
    post: Option[Array[Byte]] = None,
    isJson: Boolean = false
  ): Int = {
    if verbosity >= 1 && post.isEmpty then Console.err.println(s"Getting $url")
    else if verbosity >= 1 then Console.err.println(s"Posting to $url")
    val req  = request(url, post, isJson)
    val resp =
      try req.send(backend)
      catch {
        case e: Throwable =>
          if verbosity >= 1 then Console.err.println(s"Failed to get $url: ${e.getMessage}")
          if verbosity >= 2 then e.printStackTrace()
          throw e
      }
    if verbosity >= 1 then Console.err.println(s"Done: $url")

    if resp.code.isSuccess then resp.code.code
    else {
      val msg =
        s"Failed to get $url (http status: ${resp.code.code}, response: ${Try(new String(resp.body, StandardCharsets.UTF_8)).getOrElse("")})"
      if resp.code.isClientError then throw new FileNotFoundException(msg)
      else throw new Exception(msg)
    }
  }

  def get[T: JsonValueCodec](
    url: String,
    post: Option[Array[Byte]] = None,
    nested: Boolean = true,
    isJson: Boolean = false
  ): T = {
    if verbosity >= 1 && post.isEmpty then Console.err.println(s"Getting $url")
    else if verbosity >= 1 then Console.err.println(s"Posting to $url")
    val req  = request(url, post, isJson)
    val resp =
      try req.send(backend)
      catch {
        case e: Throwable =>
          if verbosity >= 1 then Console.err.println(s"Failed to get $url: ${e.getMessage}")
          if verbosity >= 2 then e.printStackTrace()
          throw e
      }
    if verbosity >= 1 then Console.err.println(s"Done: $url")

    if resp.code.isSuccess then
      if nested then
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
      if resp.code.isClientError then throw new FileNotFoundException(msg)
      else throw new Exception(msg)
    }
  }
}
