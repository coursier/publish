package coursier.publish.sonatype

import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets

import com.github.plokhotnyuk.jsoniter_scala.core._
import coursier.core.Authentication
import coursier.util.Task
import okhttp3.{MediaType, OkHttpClient, Request, RequestBody}

import scala.jdk.CollectionConverters._
import scala.util.Try
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec

final case class OkHttpClientUtil(
  client: OkHttpClient,
  authentication: Option[Authentication],
  verbosity: Int
) {

  private def request(url: String, post: Option[RequestBody] = None): Request = {
    val b = new Request.Builder().url(url)
    for (body <- post)
      b.post(body)

    // Handling this ourselves rather than via client.setAuthenticator / com.squareup.okhttp.Authenticator
    for (auth <- authentication; (k, v) <- auth.allHttpHeaders)
      b.addHeader(k, v)

    // ???
    b.addHeader(
      "Accept",
      "application/json,application/vnd.siesta-error-v1+json,application/vnd.siesta-validation-errors-v1+json"
    )

    val r = b.build()

    if (verbosity >= 2) {
      val m = r.headers().toMultimap.asScala.mapValues(_.asScala.toVector)
      for ((k, l) <- m; v <- l)
        System.err.println(s"$k: $v")
    }

    r
  }

  def postBody(content: Array[Byte]): RequestBody =
    RequestBody.create(OkHttpClientUtil.mediaType, content)

  def create(url: String, post: Option[RequestBody] = None): Unit = {

    if (verbosity >= 1)
      Console.err.println(s"Getting $url")
    val resp = client.newCall(request(url, post)).execute()
    if (verbosity >= 1)
      Console.err.println(s"Done: $url")

    if (resp.code() != 201)
      throw new Exception(
        s"Failed to get $url (http status: ${resp.code()}, response: ${Try(resp.body().string()).getOrElse("")})"
      )
  }

  def get[T: JsonValueCodec](
    url: String,
    post: Option[RequestBody] = None,
    nested: Boolean = true
  ): T = {

    if (verbosity >= 1)
      Console.err.println(s"Getting $url")
    if (verbosity >= 2)
      post.foreach { b =>
        val buf = new okio.Buffer
        b.writeTo(buf)
        System.err.println("Sending " + buf)
      }
    val resp = client.newCall(request(url, post)).execute()
    if (verbosity >= 1)
      Console.err.println(s"Done: $url")

    if (resp.isSuccessful)
      if (nested)
        try readFromArray[T](resp.body().bytes())
        catch {
          case e: JsonReaderException =>
            throw new Exception(s"Received invalid response from $url", e)
        }
      else
        try readFromArray[T](resp.body().bytes())
        catch {
          case e: JsonReaderException =>
            throw new Exception(s"Received invalid response from $url", e)
        }
    else {
      val msg =
        s"Failed to get $url (http status: ${resp.code()}, response: ${Try(resp.body().string()).getOrElse("")})"
      val notFound = resp.code() / 100 == 4
      if (notFound)
        throw new FileNotFoundException(msg)
      else
        throw new Exception(msg)
    }
  }
}

object OkHttpClientUtil {

  private val mediaType = MediaType.parse("application/json")

}
