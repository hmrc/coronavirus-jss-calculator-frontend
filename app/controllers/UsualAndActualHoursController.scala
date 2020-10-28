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

import java.time.LocalDate
import java.time.temporal.ChronoUnit
import java.util.stream.Collectors

import controllers.actions._
import forms.UsualAndActualHoursFormProvider
import javax.inject.Inject
import models.requests.DataRequest
import models.{BusinessClosedWithDates, NormalMode, Period, PeriodWithHours, TemporaryWorkingAgreementWithDates, UserAnswers, UsualAndActualHours}
import navigation.Navigator
import pages._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import services.RegularPayGrantCalculator
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.UsualAndActualHoursView

import scala.collection.JavaConverters._
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
        case None        => form
        case Some(value) => form.fill(value)
      }

      val workPeriod = request.userAnswers.get(SelectWorkPeriodsPage).flatMap(_.lift(idx - 1)) match {
        case Some(period) => period
        case None         => throw new RuntimeException(s"expected WorkPeriod at index: ${idx - 1}, but it doesn't exist")
      }

      if (isPeriodEligibleForHours(request.userAnswers, workPeriod)) {
        val (startDateToShow, endDateToShow) = getStartAndEndDatesToShow(idx - 1, request)
        Future.successful(Ok(view(preparedForm, idx, startDateToShow, endDateToShow)))
      } else {
        //store u hours in mongo and proceed to next page in the loop
        for {
          updatedAnswers <-
            Future.fromTry(request.userAnswers.set(UsualAndActualHoursPage, UsualAndActualHours(0.0, 0.0), Some(idx)))
          _              <- sessionRepository.set(updatedAnswers)
        } yield Redirect(navigator.nextPage(UsualAndActualHoursPage, NormalMode, updatedAnswers, Some(idx)))
      }
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors => {
            val (startDateToShow, endDateToShow) = getStartAndEndDatesToShow(idx - 1, request)
            Future.successful(BadRequest(view(formWithErrors, idx, startDateToShow, endDateToShow)))
          },
          value =>
            for {
              updatedAnswers <- Future.fromTry(request.userAnswers.set(UsualAndActualHoursPage, value, Some(idx)))
              _              <- sessionRepository.set(updatedAnswers)
            } yield Redirect(navigator.nextPage(UsualAndActualHoursPage, NormalMode, updatedAnswers, Some(idx)))
        )
  }

  private def getStartAndEndDatesToShow(idx: Int, request: DataRequest[_]) = {

    val workPeriod = request.userAnswers.get(SelectWorkPeriodsPage).flatMap(_.lift(idx)) match {
      case Some(period) => period
      case None         => throw new RuntimeException(s"expected WorkPeriod at index: $idx, but it doesn't exist")
    }

    val supportClaimPeriod = request.userAnswers.get(ClaimPeriodPage) match {
      case Some(cp) => cp.supportClaimPeriod
      case None     => throw new RuntimeException(s"expected ClaimPeriod at index: $idx, but it doesn't exist")
    }

    val startDateToShow =
      if (workPeriod.startDate.isBefore(supportClaimPeriod.startDate)) supportClaimPeriod.startDate
      else workPeriod.startDate

    val endDateToShow = workPeriod.endDate

    (startDateToShow, endDateToShow)
  }

  private def isPeriodEligibleForHours(userAnswers: UserAnswers, workPeriod: Period): Boolean = {

    val stwaPeriods = userAnswers.getList(ShortTermWorkingAgreementPeriodPage)
    val bcPeriods   = userAnswers.getList(BusinessClosedPeriodsPage)

    //FIXME: Hacky way to pass 0.0s for hours to see if Twa intercepts PP
    val periodWithHours = PeriodWithHours(workPeriod.startDate, workPeriod.endDate, 0.0, 0.0)

    val twaDaysInPayPeriod = totalNumberOfTwaDaysInPayPeriod(periodWithHours, stwaPeriods)

    //No TWA days in PP
    if (twaDaysInPayPeriod == 0) {
      false
    } else { //TWA days exist in PP
      //is TWA fully covered by BC
      val overlappingTwaInPayPeriod =
        stwaPeriods.filter(p =>
          isDateInteractsPeriod(workPeriod.startDate, Period(p.startDate, p.endDate))
            || isDateInteractsPeriod(workPeriod.endDate, Period(p.startDate, p.endDate))
        )

      val overlappingBcInPayPeriod =
        bcPeriods.filter(p =>
          isDateInteractsPeriod(workPeriod.startDate, Period(p.startDate, p.endDate))
            || isDateInteractsPeriod(workPeriod.endDate, Period(p.startDate, p.endDate))
        )

      val hasBusinessClosedCoversAllTWA =
        businessClosedCoversAllTWA(overlappingTwaInPayPeriod, overlappingBcInPayPeriod)

      if (hasBusinessClosedCoversAllTWA) {
        false
      } else {
        true
      }
    }
  }

  private def businessClosedCoversAllTWA(
    twaPeriods: List[TemporaryWorkingAgreementWithDates],
    bcPeriods: List[BusinessClosedWithDates]
  ) = {

    val twaDays = sortedTWA(twaPeriods).flatMap(d => datesIn(d.startDate, d.endDate))
    val bcDays  = sortedBusinessClosed(bcPeriods).flatMap(d => datesIn(d.startDate, d.endDate))

    val unCoveredTwaInBC = twaDays.filter(d => !bcDays.contains(d))

    unCoveredTwaInBC.isEmpty
  }

  private def isDateInteractsPeriod(date: LocalDate, period: Period) =
    date.compareTo(period.startDate) >= 0 && date.compareTo(period.endDate) <= 0;

  private def datesIn(startDate: LocalDate, endDate: LocalDate) =
    java.util.stream.Stream
      .iterate[LocalDate](startDate, date => date.plusDays(1))
      .limit(ChronoUnit.DAYS.between(startDate, endDate.plusDays(1)))
      .collect(Collectors.toList())
      .asScala
      .toList

}
