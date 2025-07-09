package coursier.publish.sonatype

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*
import coursier.core.Authentication
import coursier.publish.sonatype.logger.SonatypeLogger
import coursier.publish.util.EmaRetryParams
import coursier.util.Task
import sttp.client3.*

import java.nio.charset.StandardCharsets
import java.util.concurrent.ScheduledExecutorService

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, DurationInt}
import scala.util.Try
import scala.util.control.NonFatal

final case class SonatypeApi(
  backend: SttpBackend[Identity, Any],
  base: String,
  authentication: Option[Authentication],
  verbosity: Int,
  retryOnTimeout: Int = 3,
  stagingRepoRetryParams: EmaRetryParams = EmaRetryParams(3, 10.seconds.toMillis, 2.0f)
) {

  // vaguely inspired by https://github.com/lihaoyi/mill/blob/7b4ced648ecd9b79b3a16d67552f0bb69f4dd543/scalalib/src/mill/scalalib/publish/SonatypeHttpApi.scala
  // and https://github.com/xerial/sbt-sonatype/blob/583db138df2b0e7bbe58717103f2c9874fca2a74/src/main/scala/xerial/sbt/Sonatype.scala

  import SonatypeApi.*

  private def postBody(content: Array[Byte]): Array[Byte] =
    """{"data":""".getBytes(StandardCharsets.UTF_8) ++ content ++ "}".getBytes(
      StandardCharsets.UTF_8
    )

  private def get[T: JsonValueCodec](
    url: String,
    post: Option[Array[Byte]] = None,
    nested: Boolean = true,
    isJson: Boolean = false
  ): T =
    if nested then clientUtil.get(url, post, nested, isJson)(using Response.codec[T]).data
    else clientUtil.get[T](url, post, nested, isJson)

  private val clientUtil = HttpClientUtil(backend, authentication, verbosity)

  private def withRetry[T](task: Int => Task[T]): Task[T] = {

    def helper(attempt: Int): Task[T] =
      task(attempt).attempt.flatMap {
        case Left(_: java.net.SocketTimeoutException) if attempt + 1 < retryOnTimeout =>
          helper(attempt + 1)
        case other =>
          Task.fromEither(other)
      }

    helper(0)
  }

  def listProfiles(logger: SonatypeLogger = SonatypeLogger.nop): Task[Seq[SonatypeApi.Profile]] = {

    // for w/e reasons, Profiles.Profile.decode isn't implicitly picked
    def task(attempt: Int) = Task.delay {
      logger.listingProfiles(attempt, retryOnTimeout)
      val res =
        try get(url = s"$base/staging/profiles", isJson = true)(using Profiles.Profile.listCodec)
            .map(_.profile)
        catch {
          case NonFatal(e) =>
            logger.listedProfiles(Some(e))
            throw e
        }
      logger.listedProfiles(None)
      res
    }

    withRetry(task)
  }

  def rawListProfiles(): RawJson =
    get(s"$base/staging/profiles")(using RawJson.codec)

  def decodeListProfilesResponse(json: RawJson): Either[Exception, Seq[SonatypeApi.Profile]] =
    try {
      val l = readFromArray(json.value)(Profiles.Profile.listCodec)
      Right(l.map(_.profile))
    }
    catch {
      case e: JsonReaderException =>
        Left(new Exception("Error decoding response", e))
    }

  def listProfileRepositories(profileIdOpt: Option[String]): Seq[SonatypeApi.Repository] =
    get(
      url = s"$base/staging/profile_repositories" + profileIdOpt.fold("")("/" + _),
      isJson = true
    )(
      using RepositoryResponse.listCodec
    ).map(_.repository)

  def rawListProfileRepositories(profileIdOpt: Option[String]): RawJson =
    get(
      url = s"$base/staging/profile_repositories" + profileIdOpt.fold("")("/" + _),
      isJson = true
    )(using RawJson.codec)

  def decodeListProfileRepositoriesResponse(json: RawJson)
    : Either[Exception, Seq[SonatypeApi.Repository]] =
    try Right(readFromArray(json.value)(RepositoryResponse.listCodec).map(_.repository))
    catch {
      case e: JsonReaderException =>
        Left(new Exception("Error decoding response", e))
    }

  def createStagingRepository(profile: Profile, description: String): String = {
    Console.err.println(
      s"Creating staging repository for profile ${profile.name} (id: ${profile.id}) with description: $description"
    )
    get(
      s"$base/staging/profiles/${profile.id}/start",
      post = Some(postBody(writeToArray(StartRequest(description)))),
      isJson = true
    )(using StartResponse.codec).stagedRepositoryId
  }

  def rawCreateStagingRepository(profile: Profile, description: String): RawJson =
    get(
      s"${profile.uri}/start",
      post = Some(postBody(writeToArray(StartRequest(description)))),
      isJson = true
    )(using RawJson.codec)

  private def stagedRepoAction(
    action: String,
    profile: Profile,
    repositoryId: String,
    description: String
  ): Unit = {
    val url  = s"$base/staging/profiles/${profile.id}/$action"
    val body = postBody(writeToArray(StagedRepositoryRequest(description, repositoryId)))
    @tailrec
    def sendRequest(attempt: Int, waitDurationMs: Long): Unit = {
      val resp = clientUtil.createResponse(url, post = Some(body), isJson = true)

      if !resp.code.isSuccess then {
        if attempt >= stagingRepoRetryParams.attempts then
          throw new Exception(
            s"Failed to get $url (http status: ${resp.code.code}, response: ${Try(new String(resp.body, StandardCharsets.UTF_8)).getOrElse("")})"
          )

        Thread.sleep(waitDurationMs)
        sendRequest(attempt + 1, stagingRepoRetryParams.update(waitDurationMs))
      }
    }

    sendRequest(1, stagingRepoRetryParams.initialWaitDurationMs)
  }

  def sendCloseStagingRepositoryRequest(
    profile: Profile,
    repositoryId: String,
    description: String
  ): Unit = stagedRepoAction("finish", profile, repositoryId, description)

  @deprecated
  def sendPromoteStagingRepositoryRequest(
    profile: Profile,
    repositoryId: String,
    description: String
  ): Unit = stagedRepoAction("promote", profile, repositoryId, description)

  def promoteStagingRepository(repositoryId: String, description: String): Int = {
    if verbosity >= 1 then
      Console.err.println(
        s"Promoting staging repository for repository: $repositoryId with description: $description"
      )
    clientUtil.getEmptyResponse(
      url = s"$base/staging/bulk/promote",
      post = Some(postBody(writeToArray(PromoteRequest(
        stagedRepositoryIds = Seq(repositoryId),
        description = description,
        autoDropAfterRelease = true
      )))),
      isJson = true
    )
  }

  def sendDropStagingRepositoryRequest(
    profile: Profile,
    repositoryId: String,
    description: String
  ): Unit = stagedRepoAction("drop", profile, repositoryId, description)

  def lastActivity(repositoryId: String, action: String): Option[RawJson] =
    get(s"$base/staging/repository/$repositoryId/activity", nested = false)(
      using SonatypeApi.jsonListCodec
    )
      .filter { json =>
        val nameOpt = readFromArray(json.value)(SonatypeApi.MaybeHasName.codec).name
        nameOpt.contains(action)
      }
      .lastOption

  def waitForStatus(
    profileId: String,
    repositoryId: String,
    status: String,
    checkActivityOpt: Option[String],
    maxAttempt: Int,
    initialDelay: Duration,
    backoffFactor: Double,
    es: ScheduledExecutorService
  ): Task[Unit] = {
    // TODO Stop early in case of error (which statuses exactly???)
    def task(attempt: Int, nextDelay: Duration, totalDelay: Duration): Task[Unit] =
      Task.delay(listProfileRepositories(Some(profileId))).flatMap { l =>
        l.find(_.id == repositoryId) match {
          case None =>
            Task.fail(new Exception(s"Repository $repositoryId not found"))
          case Some(repo) =>
            // TODO Use logger for that
            System.err.println(s"Repository $repositoryId has status ${repo.`type`}")
            repo.`type` match {
              case `status` =>
                Task.point(())
              case other =>
                if attempt < maxAttempt then {
                  val errors = checkActivityOpt match {
                    case Some(checkActivity) =>
                      val lastActivity0 = lastActivity(repositoryId, checkActivity)
                      lastActivity0.toSeq.map(activityErrored).flatMap(_.left.toOption)
                    case None =>
                      Nil
                  }
                  if errors.isEmpty then
                    task(attempt + 1, nextDelay * backoffFactor, totalDelay + nextDelay)
                      .schedule(nextDelay, es)
                  else
                    Task.fail(
                      new Exception(
                        s"Error waiting for repository $repositoryId to be $status: ${errors.mkString(", ")}"
                      )
                    )
                }
                else
                  // FIXME totalDelay doesn't include the duration of the requests themselves (only the time between)
                  Task.fail(
                    new Exception(s"Repository $repositoryId in state $other after $totalDelay")
                  )
            }
        }
      }

    task(1, initialDelay, Duration.Zero)
  }

}

object SonatypeApi {
  final case class Profile(
    id: String,
    name: String,
    uri: String
  )

  final case class Repository(
    profileId: String,
    profileName: String,
    id: String,
    `type`: String
  )

  def activityErrored(activity: RawJson): Either[List[String], Unit] =
    try {
      val a      = readFromArray(activity.value)(Activity.codec)
      val errors = a.events
        .filter(_.severity >= 1)
        .map(e => e.propertiesMap.getOrElse("failureMessage", e.name))
      if errors.isEmpty then Right(()) else Left(errors)
    }
    catch {
      case e: JsonReaderException =>
        throw new Exception("Error decoding activity", e)
    }

  // same kind of check as sbt-sonatype
  def repositoryClosed(activity: RawJson, repoId: String): Boolean =
    try {
      val a = readFromArray(activity.value)(Activity.codec)
      a.events.exists { e =>
        e.name == "repositoryClosed" &&
        e.properties.exists(p => p.name == "id" && p.value == repoId)
      }
    }
    catch {
      case e: JsonReaderException =>
        throw new Exception("Error decoding activity", e)
    }
  def repositoryPromoted(activity: RawJson, repoId: String): Boolean =
    try {
      val a = readFromArray(activity.value)(Activity.codec)
      a.events.exists { e =>
        e.name == "repositoryReleased" &&
        e.properties.exists(p => p.name == "id" && p.value == repoId)
      }
    }
    catch {
      case e: JsonReaderException =>
        throw new Exception("Error decoding activity", e)
    }

  private final case class Activity(name: String, events: List[Activity.Event])

  private object Activity {
    final case class Event(name: String, severity: Int, properties: List[Property]) {
      lazy val propertiesMap: Map[String, String] =
        properties.map(p => (p.name, p.value)).toMap
    }
    final case class Property(name: String, value: String)
    val codec: JsonValueCodec[Activity] = JsonCodecMaker.make
  }

  private final case class Response[T](data: T)

  private object Response {
    def codec[T: JsonValueCodec]: JsonValueCodec[Response[T]] =
      JsonCodecMaker.make
  }

  private object Profiles {

    final case class Profile(
      id: String,
      name: String,
      resourceURI: String
    ) {
      def profile: SonatypeApi.Profile =
        SonatypeApi.Profile(
          id,
          name,
          resourceURI
        )
    }

    object Profile {
      val listCodec: JsonValueCodec[List[Profile]] = JsonCodecMaker.make
    }
  }

  private final case class RepositoryResponse(
    profileId: String,
    profileName: String,
    repositoryId: String,
    `type`: String
  ) {
    def repository: Repository =
      Repository(
        profileId,
        profileName,
        repositoryId,
        `type`
      )
  }
  private object RepositoryResponse {
    val listCodec: JsonValueCodec[List[RepositoryResponse]] =
      JsonCodecMaker.make
  }

  private final case class StartRequest(description: String)
  private object StartRequest {
    implicit val codec: JsonValueCodec[StartRequest] = JsonCodecMaker.make
  }

  private final case class PromoteRequest(
    stagedRepositoryIds: Seq[String],
    description: String,
    autoDropAfterRelease: Boolean
  )
  private object PromoteRequest {
    implicit val codec: JsonValueCodec[PromoteRequest] = JsonCodecMaker.make
  }

  private final case class PromoteResponse(
    stagedRepositoryIds: Seq[String],
    description: String,
    transitioning: Boolean
  )
  private object PromoteResponse {
    val codec: JsonValueCodec[PromoteResponse] = JsonCodecMaker.make
  }
  private final case class StartResponse(stagedRepositoryId: String)
  private object StartResponse {
    val codec: JsonValueCodec[StartResponse] = JsonCodecMaker.make
  }

  private final case class StagedRepositoryRequest(
    description: String,
    stagedRepositoryId: String
  )
  private object StagedRepositoryRequest {
    implicit val codec: JsonValueCodec[StagedRepositoryRequest] = JsonCodecMaker.make
  }

  private val jsonListCodec: JsonValueCodec[List[RawJson]] = JsonCodecMaker.make
  private final case class MaybeHasName(name: Option[String])
  private object MaybeHasName {
    val codec: JsonValueCodec[MaybeHasName] = JsonCodecMaker.make
  }
}
