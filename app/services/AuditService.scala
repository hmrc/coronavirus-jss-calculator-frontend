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
import models.{BusinessClosedPeriod, JobSupport, Period, SupportBreakdown, TemporaryWorkingAgreementPeriod, UserAnswers, UsualAndActualHours}
import pages._
import play.api.libs.json.{Format, Json}
import play.api.mvc.Request
import services.JobSupportSchemeCalculatorEvent.JobSupportSchemeCalculatorEvent
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.audit.AuditExtensions._
import uk.gov.hmrc.play.audit.http.connector.AuditConnector
import uk.gov.hmrc.play.audit.model.ExtendedDataEvent

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object JobSupportSchemeCalculatorEvent extends Enumeration {
  type JobSupportSchemeCalculatorEvent = Value

  val CalculationPerformed, CalculationFailed = Value
}

case class UserAnswersAuditDetails(
  supportClaimPeriod: String,
  payFrequency: String,
  lastPayDate: String,
  endPayDate: String,
  payMethod: String,
  payPeriods: String,
  selectPayPeriods: List[Period],
  regularPayAmount: Double,
  temporaryWorkingAgreement: String,
  temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
  businessClosed: String,
  businessClosedPeriods: List[BusinessClosedPeriod],
  usualAndActualWorkingHours: List[UsualAndActualHours]
)

object UserAnswersAuditDetails {
  def apply(userAnswers: UserAnswers): UserAnswersAuditDetails =
    new UserAnswersAuditDetails(
      userAnswers.get(ClaimPeriodPage).map(_.toString).getOrElse(""),
      userAnswers.get(PayFrequencyPage).map(_.toString).getOrElse(""),
      userAnswers.get(LastPayDatePage).map(_.toString).getOrElse(""),
      userAnswers.get(EndPayDatePage).map(_.toString).getOrElse(""),
      userAnswers.get(PayMethodPage).map(_.toString).getOrElse(""),
      userAnswers.get(PayPeriodsPage).map(_.toString).getOrElse(""),
      userAnswers.get(SelectWorkPeriodsPage).getOrElse(List.empty),
      userAnswers.get(RegularPayAmountPage).map(_.value.toDouble).getOrElse(0.0),
      userAnswers.get(TemporaryWorkingAgreementPage).map(_.toString).getOrElse(""),
      userAnswers.getList(ShortTermWorkingAgreementPeriodPage),
      userAnswers.get(BusinessClosedPage).map(_.toString).getOrElse(""),
      userAnswers.getList(BusinessClosedPeriodsPage),
      userAnswers.getList(UsualAndActualHoursPage)
    )

  implicit val format: Format[UserAnswersAuditDetails] = Json.format
}

case class JobSupportAuditDetails(
  periodSupport: List[SupportBreakdown],
  referenceSalary: Double,
  isIneligible: Boolean,
  totalEmployeeSalary: Double,
  totalEmployersGrant: Double,
  totalClosed: Double,
  totalGrant: Double
)

object JobSupportAuditDetails {
  def apply(jobSupport: JobSupport): JobSupportAuditDetails =
    new JobSupportAuditDetails(
      jobSupport.supportBreakdown,
      jobSupport.referenceSalary,
      jobSupport.isIneligible,
      jobSupport.totalEmployeeSalary,
      jobSupport.totalEmployersGrant,
      jobSupport.totalClosed,
      jobSupport.totalGrant
    )

  implicit val format: Format[JobSupportAuditDetails] = Json.format
}

case class JssDetails(userAnswers: UserAnswersAuditDetails, calculationResult: JobSupportAuditDetails)

object JssDetails {
  implicit val format: Format[JssDetails] = Json.format
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
      JssDetails(UserAnswersAuditDetails(userAnswers), JobSupportAuditDetails(jobSupport))
    )

  private def auditEvent(
    calculatorEvent: JobSupportSchemeCalculatorEvent,
    transactionName: String,
    details: JssDetails
  )(implicit hc: HeaderCarrier, request: Request[Any], ec: ExecutionContext): Future[Unit] = {
    val tags  = hc.toAuditTags(transactionName, request.path)
    val event = ExtendedDataEvent(
      auditSource = config.appName,
      auditType = calculatorEvent.toString,
      tags = tags,
      detail = Json.toJson(details)
    )
    send(event)
  }

  private def send(events: ExtendedDataEvent*)(implicit hc: HeaderCarrier, ec: ExecutionContext): Future[Unit] =
    Future {
      events.foreach { event =>
        Try(auditConnector.sendExtendedEvent(event))
      }
    }

}
