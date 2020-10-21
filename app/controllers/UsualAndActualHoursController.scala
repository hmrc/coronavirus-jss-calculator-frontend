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
import models.NormalMode
import models.requests.DataRequest
import navigation.Navigator
import pages.{ClaimPeriodPage, SelectWorkPeriodsPage, UsualAndActualHoursPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.UsualAndActualHoursView

import scala.concurrent.{ExecutionContext, Future}

class UsualAndActualHoursController @Inject()(
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
    extends FrontendBaseController with I18nSupport {

  private val form = formProvider()

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(UsualAndActualHoursPage, Some(idx)) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    val (startDateToShow, endDateToShow) = getStartAndEndDatesToShow(idx - 1, request)

    Ok(view(preparedForm, idx, startDateToShow, endDateToShow))
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
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
      if (workPeriod.startDate.isBefore(supportClaimPeriod.startDate)) supportClaimPeriod.startDate else workPeriod.startDate

    val endDateToShow = workPeriod.endDate

    (startDateToShow, endDateToShow)
  }
}
