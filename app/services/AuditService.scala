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

package services

import config.FrontendAppConfig
import javax.inject.{Inject, Singleton}
import models.{JobSupport, UserAnswers}
import pages._
import play.api.libs.json.{JsBoolean, JsNumber, JsString, Json}
import play.api.mvc.Request
import services.JobSupportSchemeCalculatorEvent.JobSupportSchemeCalculatorEvent
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.DataEvent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object JobSupportSchemeCalculatorEvent extends Enumeration {
  type JobSupportSchemeCalculatorEvent = Value

  val CalculationPerformed, CalculationFailed = Value
}

@Singleton
class AuditService @Inject() (auditConnector: AuditConnector, config: FrontendAppConfig) {

  def sendCalculationPerformed(userAnswers: UserAnswers, jobSupport: JobSupport)(implicit
    hc: HeaderCarrier,
    request: Request[Any],
    ec: ExecutionContext
  ): Future[Unit] =
    auditEvent(
      JobSupportSchemeCalculatorEvent.CalculationPerformed,
      "calculation-performed",
      Seq(
        "userAnswers"       -> userAnswersTransformer(userAnswers),
        "calculationResult" -> jobSupportTransformer(jobSupport)
      )
    )

  implicit def toJson[A](userAnswer: Option[A]) = JsString(userAnswer.map(_.toString).getOrElse(""))

  private def userAnswersTransformer(userAnswers: UserAnswers) =
    Json.obj(
      "supportClaimPeriod"               -> toJson(userAnswers.get(ClaimPeriodPage)),
      "payFrequency"                     -> toJson(userAnswers.get(PayFrequencyPage)),
      "lastPayDate"                      -> toJson(userAnswers.get(LastPayDatePage)),
      "endPayDate"                       -> toJson(userAnswers.get(EndPayDatePage)),
      "payMethod"                        -> toJson(userAnswers.get(PayMethodPage)),
      "payPeriods"                       -> toJson(userAnswers.get(PayPeriodsPage)),
      "selectPayPeriods"                 -> Json.toJson(userAnswers.get(SelectWorkPeriodsPage)),
      "regularPayAmount"                 -> toJson(userAnswers.get(RegularPayAmountPage).map(_.value.toDouble)),
      "temporaryWorkingAgreement"        -> toJson(userAnswers.get(TemporaryWorkingAgreementPage)),
      "temporaryWorkingAgreementPeriods" -> Json.toJson(userAnswers.getList(ShortTermWorkingAgreementPeriodPage)),
      "businessClosed"                   -> toJson(userAnswers.get(BusinessClosedPage)),
      "businessClosedPeriods"            -> Json.toJson(userAnswers.getList(BusinessClosedPeriodsPage)),
      "UsualAndActualWorkingHours"       -> Json.toJson(userAnswers.getList(UsualAndActualHoursPage))
    )

  private def jobSupportTransformer(jobSupport: JobSupport) =
    Json.obj(
      "periodSupport"       -> Json.toJson(jobSupport.periodSupport),
      "referenceSalary"     -> JsNumber(jobSupport.referenceSalary),
      "isEligible"          -> JsBoolean(jobSupport.isEligible),
      "totalEmployeeSalary" -> JsNumber(jobSupport.totalEmployeeSalary),
      "totalEmployersGrant" -> JsNumber(jobSupport.totalEmployersGrant),
      "totalClosed"         -> JsNumber(jobSupport.totalClosed),
      "totalGrant"          -> JsNumber(jobSupport.totalClosed)
    )

  private def auditEvent(
    event: JobSupportSchemeCalculatorEvent,
    transactionName: String,
    details: Seq[(String, Any)]
  )(implicit hc: HeaderCarrier, request: Request[Any], ec: ExecutionContext): Future[Unit] =
    send(createEvent(event, transactionName, details: _*))

  private def createEvent(
    event: JobSupportSchemeCalculatorEvent,
    transactionName: String,
    details: (String, Any)*
  )(implicit hc: HeaderCarrier, request: Request[Any]): DataEvent = {

    val detail = hc.toAuditDetails(details.map(pair => pair._1 -> pair._2.toString): _*)
    val tags   = hc.toAuditTags(transactionName, request.path)
    DataEvent(auditSource = config.appName, auditType = event.toString, tags = tags, detail = detail)
  }

  private def send(events: DataEvent*)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future {
      events.foreach { event =>
        Try(auditConnector.sendEvent(event))
      }
    }

}
