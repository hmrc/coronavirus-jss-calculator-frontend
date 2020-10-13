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
import com.google.inject.Inject
import config.FrontendAppConfig
import play.api.mvc.Results.NotFound
import play.api.mvc.{Call, RequestHeader, Result}
import uk.gov.hmrc.whitelist.AkamaiWhitelistFilter

import scala.concurrent.Future

class WhitelistFilter @Inject() (
                                  config: FrontendAppConfig,
                                  override val mat: Materializer,
                                  view: views.html.whitelistFilter.TaxServiceGovUkNotFound
                                ) extends AkamaiWhitelistFilter {

  override val whitelist: Seq[String] = {
    config.whitelistIps
      .split(",")
      .map(_.trim)
      .filter(_.nonEmpty)
  }

  override def response: Result = NotFound(view())

  override val destination: Call = Call("GET", config.whitelistDestination)

  override val excludedPaths: Seq[Call] = {
    config.whitelistExcluded.split(",").map {
      path =>
        Call("GET", path.trim)
    }
  }

  override def apply(requestFunc: RequestHeader => Future[Result])(requestHeader: RequestHeader): Future[Result] = {
    if (config.whitelistEnabled) super.apply(requestFunc)(requestHeader) else requestFunc(requestHeader)
  }
}
