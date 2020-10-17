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
import pages.{SelectWorkPeriodsPage, UsualAndActualHoursPage}
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

    Ok(view(preparedForm, idx, getWorkPeriodAtIdx(idx - 1, request)))
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => Future.successful(BadRequest(view(formWithErrors, idx, getWorkPeriodAtIdx(idx - 1, request)))),
        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(UsualAndActualHoursPage, value, Some(idx)))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(navigator.nextPage(UsualAndActualHoursPage, NormalMode, updatedAnswers, Some(idx)))
      )
  }

  private def getWorkPeriodAtIdx(idx: Int, request: DataRequest[_]) =
    request.userAnswers.get(SelectWorkPeriodsPage).flatMap(_.lift(idx)) match {
      case Some(period) => period
      case None         => throw new RuntimeException(s"expected WorkPeriod ar index: $idx, but it doesn't exist")
    }
}
