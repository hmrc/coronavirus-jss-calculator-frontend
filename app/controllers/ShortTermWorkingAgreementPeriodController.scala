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
import forms.ShortTermWorkingAgreementPeriodFormProvider
import javax.inject.Inject
import models.AddMore
import navigation.Navigator
import pages.ShortTermWorkingAgreementPeriodPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.ShortTermWorkingAgreementPeriodView

import scala.concurrent.{ExecutionContext, Future}

class ShortTermWorkingAgreementPeriodController @Inject()(
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: ShortTermWorkingAgreementPeriodFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: ShortTermWorkingAgreementPeriodView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController with I18nSupport {

  private def form = formProvider()

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(ShortTermWorkingAgreementPeriodPage, Some(idx)) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    Ok(view(preparedForm, idx))
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, idx))),
        value => {
          val updatedAnswers = request.userAnswers.set(ShortTermWorkingAgreementPeriodPage, value, Some(idx))

          value.addMore match {
            case AddMore.Yes =>
              for {
                updated <- Future.fromTry(updatedAnswers)
                _       <- sessionRepository.set(updated)
              } yield Redirect(routes.ShortTermWorkingAgreementPeriodController.onPageLoad(idx + 1))

            case AddMore.No =>
              val answersWithListToKeep = updatedAnswers.flatMap { answers =>
                val listToKeep = answers.getList(ShortTermWorkingAgreementPeriodPage).slice(0, idx)
                answers.setList(ShortTermWorkingAgreementPeriodPage, listToKeep)
              }
              for {
                updated <- Future.fromTry(answersWithListToKeep)
                _       <- sessionRepository.set(updated)
              } yield Redirect(routes.BusinessClosedController.onPageLoad())
          }
        }
      )
  }
}
