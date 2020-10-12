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
import forms.ClaimStartDateFormProvider
import javax.inject.Inject
import models.{Mode, NormalMode, UserAnswers}
import navigation.Navigator
import pages.ClaimStartDatePage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.ClaimStartDateView

import scala.concurrent.{ExecutionContext, Future}

class ClaimStartDateController @Inject()(
    override val messagesApi: MessagesApi,
    sessionRepository: SessionRepository,
    navigator: Navigator,
    identify: IdentifierAction,
    getData: DataRetrievalAction,
    requireData: DataRequiredAction,
    formProvider: ClaimStartDateFormProvider,
    val controllerComponents: MessagesControllerComponents,
    view: ClaimStartDateView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  def form = formProvider()

  def onPageLoad(): Action[AnyContent] = (identify andThen getData) {
    implicit request =>
      val userAnswers = request.userAnswers
        .getOrElse(UserAnswers(request.internalId))
        .get(ClaimStartDatePage)

      val preparedForm = userAnswers match {
        case Some(date) => form.fill(date)
        case None       => form
      }

      Ok(view(preparedForm))
  }

  def onSubmit(): Action[AnyContent] =
    (identify andThen getData).async { implicit request =>
      form
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors))),
          value => {
            for {
              updatedAnswers <- Future.fromTry(
                UserAnswers(request.internalId).set(ClaimStartDatePage, value))
              _ <- sessionRepository.set(updatedAnswers)
            } yield
              Redirect(
                navigator
                  .nextPage(ClaimStartDatePage, NormalMode, updatedAnswers))
          }
        )
    }
}
