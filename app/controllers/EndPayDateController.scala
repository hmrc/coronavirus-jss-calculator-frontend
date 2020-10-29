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

import controllers.actions._
import forms.EndPayDateFormProvider
import javax.inject.Inject
import models.NormalMode
import navigation.Navigator
import pages.{ClaimPeriodPage, EndPayDatePage, LastPayDatePage}
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.EndPayDateView

import scala.concurrent.{ExecutionContext, Future}

class EndPayDateController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: EndPayDateFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: EndPayDateView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private def form(lastPayDate: LocalDate, claimStartDate: LocalDate, claimEnd: LocalDate)(implicit
    messages: Messages
  ) =
    formProvider(lastPayDate, claimStartDate, claimEnd)

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    (request.userAnswers.get(LastPayDatePage), request.userAnswers.get(ClaimPeriodPage)) match {
      case (Some(lastPayDate), Some(claimPeriod)) =>
        val startDate    = claimPeriod.supportClaimPeriod.startDate
        val endDate      = claimPeriod.supportClaimPeriod.endDate
        val preparedForm = request.userAnswers.get(EndPayDatePage) match {
          case None        => form(lastPayDate, startDate, endDate)
          case Some(value) => form(lastPayDate, startDate, endDate).fill(value)
        }
        Ok(view(preparedForm, lastPayDate))

      case (None, _) => Redirect(routes.LastPayDateController.onPageLoad())
      case (_, None) => Redirect(routes.ClaimPeriodController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    (request.userAnswers.get(LastPayDatePage), request.userAnswers.get(ClaimPeriodPage)) match {
      case (Some(lastPayDate), Some(claimPeriod)) =>
        form(lastPayDate, claimPeriod.supportClaimPeriod.startDate, claimPeriod.supportClaimPeriod.endDate)
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(view(formWithErrors, lastPayDate))),
            value =>
              for {
                updatedAnswers <- Future.fromTry(request.userAnswers.set(EndPayDatePage, value))
                _              <- sessionRepository.set(updatedAnswers)
              } yield Redirect(navigator.nextPage(EndPayDatePage, NormalMode, updatedAnswers))
          )

      case (None, _) => Future.successful(Redirect(routes.LastPayDateController.onPageLoad()))
      case (_, None) => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
    }

  }
}
