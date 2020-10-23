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
import forms.BusinessClosedPeriodsFormProvider
import javax.inject.Inject
import models.NormalMode
import navigation.Navigator
import pages.BusinessClosedPeriodsPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.BusinessClosedPeriodsView

import scala.concurrent.{ExecutionContext, Future}

class BusinessClosedPeriodsController @Inject()(
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: BusinessClosedPeriodsFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: BusinessClosedPeriodsView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController with I18nSupport {

  private def form = formProvider()

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(BusinessClosedPeriodsPage, Some(idx)) match {
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
        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(BusinessClosedPeriodsPage, value, Some(idx)))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(navigator.nextPage(BusinessClosedPeriodsPage, NormalMode, updatedAnswers, Some(idx)))
      )
  }

  def remove(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    val existingPeriods = request.userAnswers.getList(BusinessClosedPeriodsPage)
    val remainingPeriods = existingPeriods.zipWithIndex.filterNot(p => p._2 == idx).map(_._1)

    for {
      updatedAnswers <- Future.fromTry(request.userAnswers.setList(BusinessClosedPeriodsPage, remainingPeriods))
      _              <- sessionRepository.set(updatedAnswers)
    } yield Redirect(navigator.nextPage(BusinessClosedPeriodsPage, NormalMode, updatedAnswers, Some(idx)))

  }
}
