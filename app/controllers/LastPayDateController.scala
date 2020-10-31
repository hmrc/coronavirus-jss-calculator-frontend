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
import forms.LastPayDateFormProvider
import javax.inject.Inject
import navigation.Navigator
import pages.{ClaimPeriodPage, LastPayDatePage}
import play.api.i18n.{I18nSupport, Messages, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.LastPayDateView

import scala.concurrent.{ExecutionContext, Future}

class LastPayDateController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  val navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: LastPayDateFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: LastPayDateView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with ControllerHelper {

  private def form(firstDayOfClaim: LocalDate)(implicit messages: Messages) = formProvider(firstDayOfClaim)

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    request.userAnswers.get(ClaimPeriodPage) match {
      case Some(claimPeriod) =>
        val firstDayOfClaim = claimPeriod.firstDayOfClaim

        val preparedForm = request.userAnswers.get(LastPayDatePage) match {
          case None        => form(firstDayOfClaim)
          case Some(value) => form(firstDayOfClaim).fill(value)
        }

        Ok(view(preparedForm, firstDayOfClaim))

      case None => Redirect(routes.ClaimPeriodController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    request.userAnswers.get(ClaimPeriodPage) match {
      case Some(claimPeriod) =>
        val firstDayOfClaim = claimPeriod.firstDayOfClaim
        form(firstDayOfClaim)
          .bindFromRequest()
          .fold(
            formWithErrors => Future.successful(BadRequest(view(formWithErrors, firstDayOfClaim))),
            value => saveAndRedirect(LastPayDatePage, request.userAnswers.set(LastPayDatePage, value))
          )

      case None => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
    }
  }
}
