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

import controllers.actions._
import forms.UsualAndActualHoursFormProvider
import javax.inject.Inject
import models.{NormalMode, PayPeriod, Period, SupportClaimPeriod, UserAnswers, UsualAndActualHours}
import navigation.Navigator
import pages._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.RegularPayGrantCalculator
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.UsualAndActualHoursView

import scala.concurrent.{ExecutionContext, Future}

class UsualAndActualHoursController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: UsualAndActualHoursFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: UsualAndActualHoursView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with RegularPayGrantCalculator
    with I18nSupport {

  private val form = formProvider()

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      val preparedForm = request.userAnswers.get(UsualAndActualHoursPage, Some(idx)) match {
        case Some(value) if (value.usualHours > 0 && value.actualHours >= 0)  => form.fill(value)
        case _ =>   form
      }

      request.userAnswers.get(ClaimPeriodPage) match {
        case Some(cp) =>
          val workPeriod = getWorkPeriodAtIdx(idx, request.userAnswers)

          if (isPeriodEligibleForHours(request.userAnswers, workPeriod, cp.supportClaimPeriod)) {
            val (startDateToShow, endDateToShow) = getStartAndEndDatesToShow(cp.supportClaimPeriod, workPeriod)
            Future.successful(Ok(view(preparedForm, idx, startDateToShow, endDateToShow)))
          } else {
            //work period not eligible for hours page - store 0.0 hours in mongo and proceed to next page in the loop
            for {
              updatedAnswers <-
                Future.fromTry(
                  request.userAnswers.set(UsualAndActualHoursPage, UsualAndActualHours(0.0, 0.0), Some(idx))
                )
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(UsualAndActualHoursPage, NormalMode, updatedAnswers, Some(idx)))
          }
        case None     => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
      }
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      request.userAnswers.get(ClaimPeriodPage) match {
        case Some(cp) =>
          form
            .bindFromRequest()
            .fold(
              formWithErrors => {
                val (startDateToShow, endDateToShow) =
                  getStartAndEndDatesToShow(cp.supportClaimPeriod, getWorkPeriodAtIdx(idx, request.userAnswers))
                Future.successful(BadRequest(view(formWithErrors, idx, startDateToShow, endDateToShow)))
              },
              value =>
                for {
                  updatedAnswers <- Future.fromTry(request.userAnswers.set(UsualAndActualHoursPage, value, Some(idx)))
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(UsualAndActualHoursPage, NormalMode, updatedAnswers, Some(idx)))
            )
        case None     => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
      }
  }

  private def getWorkPeriodAtIdx(idx: Int, userAnswers: UserAnswers) =
    userAnswers.get(SelectWorkPeriodsPage).flatMap(_.lift(idx - 1)) match {
      case Some(period) => period
      case None         =>
        throw new RuntimeException(s"expected WorkPeriod at index: ${idx - 1}, but it doesn't exist")
    }

  private def getStartAndEndDatesToShow(supportClaimPeriod: SupportClaimPeriod, workPeriod: Period) = {

    val startDateToShow =
      if (workPeriod.startDate.isBefore(supportClaimPeriod.startDate)) supportClaimPeriod.startDate
      else workPeriod.startDate

    val endDateToShow = workPeriod.endDate

    (startDateToShow, endDateToShow)
  }

  private def isPeriodEligibleForHours(
    userAnswers: UserAnswers,
    workPeriod: Period,
    supportClaimPeriod: SupportClaimPeriod
  ): Boolean = {

    val stwaPeriods                = userAnswers.getList(ShortTermWorkingAgreementPeriodPage)
    val bcPeriods                  = userAnswers.getList(BusinessClosedPeriodsPage)
    //FIXME: Hacky way to pass 0.0s
    val pp                         = PayPeriod(workPeriod.startDate, workPeriod.endDate, 0.0, 0.0)
    val closedPeriodsInPP          = getAllBusinessClosedPeriodsInThisPayPeriod(pp, bcPeriods)
    val isPPCompletelyCoveredByBCs =
      isPayPeriodCompletelyCoveredByBusinessClosedPeriod(supportClaimPeriod, pp, closedPeriodsInPP)

    if (isPPCompletelyCoveredByBCs) {
      false
    } else {
      val twasInPP                        = getAllTemporaryWorkingAgreementsInThisPayPeriod(pp, stwaPeriods)
      val isEveryTwaCompletelyCoveredByBC =
        isEveryTemporaryWorkingAgreementCompletelyCoveredABusinessClosedPeriod(twasInPP, closedPeriodsInPP)

      if (twasInPP.nonEmpty && !isEveryTwaCompletelyCoveredByBC) {
        true
      } else {
        false
      }
    }
  }
}
