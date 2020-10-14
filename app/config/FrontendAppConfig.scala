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

package config

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.RequestHeader
import uk.gov.hmrc.play.bootstrap.binders.SafeRedirectUrl

@Singleton
class FrontendAppConfig @Inject()(configuration: Configuration) {

  lazy val host: String = configuration.get[String]("host")

  private lazy val contactHost = configuration.get[String]("contact-frontend.host")
  private val contactFormServiceIdentifier = "dd-claim"

  lazy val reportAProblemPartialUrl = s"$contactHost/contact/problem_reports_ajax?service=$contactFormServiceIdentifier"
  lazy val reportAProblemNonJSUrl = s"$contactHost/contact/problem_reports_nonjs?service=$contactFormServiceIdentifier"

  def feedbackUrl(implicit request: RequestHeader): String =
    s"$contactHost/contact/beta-feedback?service=$contactFormServiceIdentifier&backUrl=${SafeRedirectUrl(host + request.uri).encodedUrl}"

  lazy val timeout: Int = configuration.get[Int]("timeout.timeout")
  lazy val countdown: Int = configuration.get[Int]("timeout.countdown")

  lazy val govukHome: String = configuration.get[String]("urls.govUkHome")
  lazy val contactByPhone: String = configuration.get[String]("urls.contactByPhone")

  lazy val languageTranslationEnabled: Boolean = configuration.get[Boolean]("features.welsh-translation")

  lazy val origin: String = configuration.get[String]("origin")

  lazy val cookies: String = host + configuration.get[String]("urls.footer.cookies")
  lazy val privacy: String = host + configuration.get[String]("urls.footer.privacy")
  lazy val termsConditions: String = host + configuration.get[String]("urls.footer.termsConditions")
  lazy val govukHelp: String = configuration.get[String]("urls.footer.govukHelp")
  lazy val daysTillAbleToClaim: Int = configuration.get[Int]("daysTillAbleToClaim")

  lazy val appName: String = configuration.get[String]("appName")

  private lazy val exitSurveyBaseUrl = configuration.get[String]("feedback-frontend.host") + configuration.get[String](
    "feedback-frontend.url")
  lazy val exitSurveyUrl = s"$exitSurveyBaseUrl/coronavirus-jss-calculator"

  lazy val whitelistEnabled: Boolean = configuration.get[Boolean]("filters.whitelist.enabled")
  lazy val whitelistDestination: String = configuration.get[String]("filters.whitelist.destination")
  lazy val whitelistExcluded: String = configuration.get[String]("filters.whitelist.excluded")
  lazy val whitelistIps: String = configuration.get[String]("filters.whitelist.ips")

  lazy val schemeEnds: String = configuration.get[String]("schemeEnds")
}
