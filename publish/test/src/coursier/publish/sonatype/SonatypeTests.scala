package coursier.publish.sonatype

import coursier.publish.util.EmaRetryParams
import sttp.client3.testing.SttpBackendStub

import utest.*

object SonatypeTests extends TestSuite {
  val tests: Tests = Tests {
    test("Retry sonatype repository actions") {
      var count       = 0
      val mockBackend = SttpBackendStub.synchronous
        .whenRequestMatches { _ =>
          count += 1; count < 6
        }
        .thenRespondServerError()
        .whenRequestMatches(_ => count >= 6)
        .thenRespondOk()

      {
        val sonatypeApi20 = SonatypeApi(
          mockBackend,
          base = "https://oss.sonatype.org",
          authentication = None,
          verbosity = 0,
          retryOnTimeout = 1,
          stagingRepoRetryParams = EmaRetryParams(20, 100L, 1.0f)
        )

        sonatypeApi20.sendPromoteStagingRepositoryRequest(
          SonatypeApi.Profile("id", "name", "uri"),
          "repo",
          "description"
        )

        assert(count == 6)
      }

      count = 0

      {
        val base         = "https://oss.sonatype.org"
        val sonatypeApi3 = SonatypeApi(
          mockBackend,
          base = base,
          authentication = None,
          verbosity = 0,
          retryOnTimeout = 1
        )

        try sonatypeApi3.sendPromoteStagingRepositoryRequest(
            SonatypeApi.Profile("id", "name", "uri"),
            "repo",
            "description"
          )
        catch {
          case e: Exception
              if e.getMessage ==
                s"Failed to get $base/staging/profiles/id/promote (http status: 500, response: Internal server error)" =>
          case t: Throwable =>
            println(s"Unexpected error: ${t.getMessage}")
            assert(false)
        }

        assert(count == 3)
      }
    }
  }
}
