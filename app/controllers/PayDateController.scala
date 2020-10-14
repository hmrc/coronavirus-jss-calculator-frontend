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

import java.time.YearMonth

import controllers.actions._
import forms.PayDateFormProvider
import javax.inject.Inject
import models.{ClaimPeriod, NormalMode}
import navigation.Navigator
import pages.{ClaimPeriodPage, PayDatePage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.PayDateView

import scala.concurrent.{ExecutionContext, Future}

class PayDateController @Inject()(
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PayDateFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: PayDateView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController with I18nSupport {

  private def form = formProvider()

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(PayDatePage) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    request.userAnswers.get(ClaimPeriodPage) match {
      case Some(claimPeriod) =>
        val firstDayOfClaim = YearMonth.parse(claimPeriod, ClaimPeriod.pattern).atDay(1)
        Ok(view(preparedForm, firstDayOfClaim))
      case None => Redirect(routes.ClaimPeriodController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => {
          request.userAnswers.get(ClaimPeriodPage) match {
            case Some(claimPeriod) =>
              val firstDayOfClaim = YearMonth.parse(claimPeriod, ClaimPeriod.pattern).atDay(1)
              Future.successful(BadRequest(view(formWithErrors, firstDayOfClaim)))
            case None => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
          }

        },
        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(PayDatePage, value))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(navigator.nextPage(PayDatePage, NormalMode, updatedAnswers))
      )
  }
}
