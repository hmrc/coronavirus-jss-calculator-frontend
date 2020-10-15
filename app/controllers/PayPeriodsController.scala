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
import forms.PayPeriodsFormProvider
import javax.inject.Inject
import models.{NormalMode, Period, UserAnswers}
import navigation.Navigator
import pages.{ClaimPeriodPage, LastPayDatePage, PayFrequencyPage, PayPeriodsPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.PayPeriodsView

import scala.concurrent.{ExecutionContext, Future}

class PayPeriodsController @Inject()(
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: PayPeriodsFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: PayPeriodsView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController with I18nSupport {

  private val form = formProvider()

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(PayPeriodsPage) match {
      case None        => form
      case Some(value) => form.fill(value)
    }

    getView(request.userAnswers, (x, y) => Ok(view(preparedForm, List.empty, x)))
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future.successful(getView(request.userAnswers, (x, y) => BadRequest(view(formWithErrors, List.empty, x))))
        },
        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(PayPeriodsPage, value))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(navigator.nextPage(PayPeriodsPage, NormalMode, updatedAnswers))
      )
  }

  private def getView(userAnswers: UserAnswers, result: (YearMonth, List[Period]) => Result) = {
    val maybeClaimPeriod = userAnswers.get(ClaimPeriodPage)
    val maybePayFrequency = userAnswers.get(PayFrequencyPage)
    val maybeLastPayDay = userAnswers.get(LastPayDatePage)

    (maybeClaimPeriod, maybePayFrequency, maybeLastPayDay) match {
      case (Some(cp), Some(pf), Some(lpd)) => result(cp.yearMonth, List.empty)
      case (None, _, _)                    => Redirect(routes.ClaimPeriodController.onPageLoad())
      case (_, None, _)                    => Redirect(routes.PayFrequencyController.onPageLoad())
      case (_, _, None)                    => Redirect(routes.LastPayDateController.onPageLoad())
    }
  }
}
