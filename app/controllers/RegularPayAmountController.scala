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
import forms.RegularPayAmountFormProvider
import javax.inject.Inject
import navigation.Navigator
import pages.{LastPayDatePage, RegularPayAmountPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.RegularPayAmountView

import scala.concurrent.{ExecutionContext, Future}

class RegularPayAmountController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  val navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: RegularPayAmountFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: RegularPayAmountView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with ControllerHelper {

  private val form = formProvider()

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(RegularPayAmountPage) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    request.userAnswers.get(LastPayDatePage) match {
      case Some(date) => Ok(view(preparedForm, date))
      case None       => Redirect(routes.LastPayDateController.onPageLoad())
    }
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => {
          val result = request.userAnswers.get(LastPayDatePage) match {
            case Some(date) => BadRequest(view(formWithErrors, date))
            case None       => Redirect(routes.LastPayDateController.onPageLoad())
          }
          Future.successful(result)
        },
        value => saveAndRedirect(RegularPayAmountPage, request.userAnswers.set(RegularPayAmountPage, value))
      )
  }
}
