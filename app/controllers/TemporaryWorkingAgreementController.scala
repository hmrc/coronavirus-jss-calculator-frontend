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
import forms.TemporaryWorkingAgreementFormProvider
import javax.inject.Inject
import models.TemporaryWorkingAgreement.writes
import navigation.Navigator
import pages.TemporaryWorkingAgreementPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.TemporaryWorkingAgreementView

import scala.concurrent.{ExecutionContext, Future}

class TemporaryWorkingAgreementController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  val navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: TemporaryWorkingAgreementFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: TemporaryWorkingAgreementView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with ControllerHelper {

  private val form = formProvider()

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(TemporaryWorkingAgreementPage) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    Ok(view(preparedForm))
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors))),
        value =>
          saveAndRedirect(TemporaryWorkingAgreementPage, request.userAnswers.set(TemporaryWorkingAgreementPage, value))
      )
  }
}
