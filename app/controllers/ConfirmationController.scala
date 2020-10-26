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

package controllers

import config.FrontendAppConfig
import controllers.actions._
import javax.inject.Inject
import models.{Period, PeriodWithHours, UsualAndActualHours}
import pages._
import play.api.Logger
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import services.{AuditService, RegularPayGrantCalculator}
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.ConfirmationView

import scala.concurrent.ExecutionContext

class ConfirmationController @Inject() (
  override val messagesApi: MessagesApi,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: ConfirmationView,
  appConfig: FrontendAppConfig,
  auditService: AuditService
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with RegularPayGrantCalculator {

  def onPageLoad: Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val workPeriods         = request.userAnswers.get(SelectWorkPeriodsPage)
    val usualAndActualHours = request.userAnswers.getList(UsualAndActualHoursPage)
    val regularPay          = request.userAnswers.get(RegularPayAmountPage)
    val payFrequency        = request.userAnswers.get(PayFrequencyPage)
    val supportClaimPeriod  = request.userAnswers.get(ClaimPeriodPage)
    val stwaDates           = request.userAnswers.getList(ShortTermWorkingAgreementPeriodPage)
    val bcDates             = request.userAnswers.getList(BusinessClosedPeriodsPage)

    (workPeriods, usualAndActualHours, payFrequency, supportClaimPeriod, regularPay) match {
      case (Some(wps), hours, Some(pf), Some(cp), Some(rp)) =>
        val jobSupport = calculateJobSupport(
          cp.supportClaimPeriod,
          periodsWithHours(wps, hours),
          stwaDates,
          bcDates,
          pf,
          rp.value.toDouble
        )

        auditService.sendCalculationPerformed(request.userAnswers, jobSupport)
        Ok(view(jobSupport, appConfig.calculatorVersion))

      case _ =>
        Logger.warn("expected data is missing from userAnswers, redirecting user to start page")
        Redirect(routes.ClaimPeriodController.onPageLoad())
    }

  }

  private def periodsWithHours(periods: List[Period], hours: List[UsualAndActualHours]) = {

    val failSafeHours = if (hours.isEmpty) {
      List.fill(periods.length)(UsualAndActualHours(0.0, 0.0))
    } else {
      hours
    }

    periods.zip(failSafeHours).map { x =>
      PeriodWithHours(x._1.startDate, x._1.endDate, x._2.usualHours, x._2.actualHours)
    }
  }
}
