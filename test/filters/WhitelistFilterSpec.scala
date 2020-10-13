/*
 * Copyright 2020 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package filters

import akka.stream.Materializer
import com.typesafe.config.ConfigException
import config.FrontendAppConfig
import generators.Generators
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers}
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.Configuration
import play.api.mvc.Call
import play.api.mvc.Results.Ok
import play.api.test.FakeRequest
import play.api.test.Helpers._

import scala.concurrent.Future

class WhitelistFilterSpec extends FreeSpec with MustMatchers with ScalaCheckPropertyChecks with MockitoSugar with Generators {

  private val mockMaterializer = mock[Materializer]

  private val otherConfigGen = Gen.mapOf[String, String](
    for {
      key   <- Gen.alphaNumStr suchThat (_.nonEmpty)
      value <- arbitrary[String]
    } yield (key, value)
  )

  private val view = new views.html.whitelistFilter.TaxServiceGovUkNotFound()

  "the list of whitelisted IP addresses" - {

    "must throw an exception" - {

      "when the underlying config value is not there" in {

        forAll(otherConfigGen, arbitrary[String], arbitrary[String]) { (otherConfig, destination, excluded) =>
          whenever(!otherConfig.contains("filters.whitelist.ips")) {

            val config = new FrontendAppConfig(
              Configuration(
                (otherConfig +
                  ("filters.whitelist.destination" -> destination) +
                  ("filters.whitelist.excluded"    -> excluded) +
                  ("claim-periods"                 -> Seq.empty)).toSeq: _*
              ))

            assertThrows[ConfigException] {
              new WhitelistFilter(config, mockMaterializer, view)
            }
          }
        }
      }
    }

    "must be empty" - {

      "when the underlying config value is empty" in {

        forAll(otherConfigGen, arbitrary[String], arbitrary[String]) { (otherConfig, destination, excluded) =>
          val config = new FrontendAppConfig(
            Configuration(
              (otherConfig +
                ("filters.whitelist.destination" -> destination) +
                ("filters.whitelist.excluded"    -> excluded) +
                ("filters.whitelist.ips"         -> "") +
                ("claim-periods"                 -> Seq.empty)).toSeq: _*
            ))

          val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

          whitelistFilter.whitelist mustBe empty
        }
      }
    }

    "must contain all of the values" - {

      "when given a comma-separated list of values" in {

        val gen = Gen.nonEmptyListOf(Gen.alphaNumStr suchThat (_.nonEmpty))

        forAll(gen, otherConfigGen, arbitrary[String], arbitrary[String]) { (ips, otherConfig, destination, excluded) =>
          val ipString = ips.mkString(",")

          val config = new FrontendAppConfig(
            Configuration(
              (otherConfig +
                ("filters.whitelist.destination" -> destination) +
                ("filters.whitelist.excluded"    -> excluded) +
                ("filters.whitelist.ips"         -> ipString) +
                ("claim-periods"                 -> Seq.empty)).toSeq: _*
            ))

          val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

          whitelistFilter.whitelist must contain theSameElementsAs ips
        }
      }
    }
  }

  "the destination for non-whitelisted visitors" - {

    "must throw an exception" - {

      "when the underlying config value is not there" in {

        forAll(otherConfigGen, arbitrary[String], arbitrary[String]) { (otherConfig, destination, excluded) =>
          whenever(!otherConfig.contains("filters.whitelist.destination")) {

            val config = new FrontendAppConfig(
              Configuration(
                (otherConfig +
                  ("filters.whitelist.ips"      -> destination) +
                  ("filters.whitelist.excluded" -> excluded) +
                  ("claim-periods"              -> Seq.empty)).toSeq: _*
              ))

            assertThrows[ConfigException] {
              new WhitelistFilter(config, mockMaterializer, view)
            }
          }
        }
      }
    }

    "must return a Call to the destination" in {

      forAll(otherConfigGen, arbitrary[String], arbitrary[String], arbitrary[String]) { (otherConfig, ips, destination, excluded) =>
        val config = new FrontendAppConfig(
          Configuration(
            (otherConfig +
              ("filters.whitelist.ips"         -> ips) +
              ("filters.whitelist.excluded"    -> excluded) +
              ("filters.whitelist.destination" -> destination) +
              ("claim-periods"                 -> Seq.empty)).toSeq: _*
          ))

        val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

        whitelistFilter.destination mustEqual Call("GET", destination)
      }
    }
  }

  "the list of excluded paths" - {

    "must throw an exception" - {

      "when the underlying config value is not there" in {

        forAll(otherConfigGen, arbitrary[String], arbitrary[String]) { (otherConfig, destination, excluded) =>
          whenever(!otherConfig.contains("filters.whitelist.excluded")) {

            val config = new FrontendAppConfig(
              Configuration(
                (otherConfig +
                  ("filters.whitelist.destination" -> destination) +
                  ("filters.whitelist.ips"         -> excluded) +
                  ("claim-periods"                 -> Seq.empty)).toSeq: _*
              ))

            assertThrows[ConfigException] {
              new WhitelistFilter(config, mockMaterializer, view)
            }
          }
        }
      }
    }

    "must return Calls to all of the values" - {

      "when given a comma-separated list of values" in {

        val gen = Gen.nonEmptyListOf(Gen.alphaNumStr suchThat (_.nonEmpty))

        forAll(gen, otherConfigGen, arbitrary[String], arbitrary[String]) { (excludedPaths, otherConfig, destination, ips) =>
          val excludedPathString = excludedPaths.mkString(",")

          val config = new FrontendAppConfig(
            Configuration(
              (otherConfig +
                ("filters.whitelist.destination" -> destination) +
                ("filters.whitelist.excluded"    -> excludedPathString) +
                ("filters.whitelist.ips"         -> ips) +
                ("claim-periods"                 -> Seq.empty)).toSeq: _*
            ))

          val expectedCalls = excludedPaths.map(Call("GET", _))

          val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

          whitelistFilter.excludedPaths must contain theSameElementsAs expectedCalls
        }
      }
    }
  }

  "the filter" - {

    "when enabled" - {

      "must allow requests from a configured IP through" in {

        val excludedPathString = "/ping/ping"

        val config = new FrontendAppConfig(
          Configuration(
            "filters.whitelist.destination" -> "",
            "filters.whitelist.excluded"    -> excludedPathString,
            "filters.whitelist.ips"         -> "123.456.789.0",
            "filters.whitelist.enabled"     -> "true",
            "claim-periods"                 -> List()
          ))

        val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

        val request = FakeRequest("GET", "/foo")
          .withHeaders("True-Client-IP" -> "123.456.789.0")

        val result = whitelistFilter.apply(_ => Future.successful(Ok("Hooray")))(request)

        status(result) mustBe OK
        contentAsString(result) mustBe "Hooray"
      }

      "must respond with NOT_FOUND for an IP not on the list" in {

        val excludedPathString = "/ping/ping"

        val config = new FrontendAppConfig(
          Configuration(
            "filters.whitelist.destination" -> "",
            "filters.whitelist.excluded"    -> excludedPathString,
            "filters.whitelist.ips"         -> "123.456.789.0",
            "filters.whitelist.enabled"     -> "true",
            "claim-periods"                 -> List()
          ))

        val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

        val request = FakeRequest("GET", "/foo")
          .withHeaders("True-Client-IP" -> "123.456.789.1")

        val result = whitelistFilter.apply(_ => Future.successful(Ok("Hooray")))(request)

        status(result) mustBe NOT_FOUND
      }

    }

    "when disabled" - {

      "must allow requests with any ip" in {

        val excludedPathString = "/ping/ping"

        val config = new FrontendAppConfig(
          Configuration(
            "filters.whitelist.destination" -> "",
            "filters.whitelist.excluded"    -> excludedPathString,
            "filters.whitelist.ips"         -> "123.456.789.0",
            "filters.whitelist.enabled"     -> "false",
            "claim-periods"                 -> List()
          ))

        val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

        val request = FakeRequest("GET", "/foo")
          .withHeaders("True-Client-IP" -> "123.456.789.1")

        val result = whitelistFilter.apply(_ => Future.successful(Ok("Hooray")))(request)

        status(result) mustBe OK
        contentAsString(result) mustBe "Hooray"
      }

      "must allow requests without a true-client-ip" in {

        val excludedPathString = "/ping/ping"

        val config = new FrontendAppConfig(
          Configuration(
            "filters.whitelist.destination" -> "",
            "filters.whitelist.excluded"    -> excludedPathString,
            "filters.whitelist.ips"         -> "123.456.789.0",
            "filters.whitelist.enabled"     -> "false",
            "claim-periods"                 -> List()
          ))

        val whitelistFilter = new WhitelistFilter(config, mockMaterializer, view)

        val request = FakeRequest("GET", "/foo")

        val result = whitelistFilter.apply(_ => Future.successful(Ok("Hooray")))(request)

        status(result) mustBe OK
        contentAsString(result) mustBe "Hooray"
      }

    }

  }
}
