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
import forms.ClaimPeriodFormProvider
import javax.inject.Inject
import models.UserAnswers
import navigation.Navigator
import pages.ClaimPeriodPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.ClaimPeriodView

import scala.concurrent.{ExecutionContext, Future}

class ClaimPeriodController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  val navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  formProvider: ClaimPeriodFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: ClaimPeriodView
)(implicit ec: ExecutionContext, appConfig: FrontendAppConfig)
    extends FrontendBaseController
    with I18nSupport
    with ControllerHelper {

  private def form = formProvider()

  def onPageLoad: Action[AnyContent] = (getSession andThen getData) { implicit request =>
    val userAnswers = request.userAnswers
      .getOrElse(UserAnswers(request.identifier))
      .get(ClaimPeriodPage)

    val preparedForm = userAnswers match {
      case Some(date) => form.fill(date)
      case None       => form
    }

    Ok(view(preparedForm))
  }

  def onSubmit: Action[AnyContent] = (getSession andThen getData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors))),
        value =>
          sessionRepository
            .clear(request.identifier)
            .flatMap(_ => saveAndRedirect(ClaimPeriodPage, UserAnswers(request.identifier).set(ClaimPeriodPage, value)))
      )
  }
}
