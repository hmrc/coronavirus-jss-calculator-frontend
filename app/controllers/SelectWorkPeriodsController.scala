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
import forms.SelectWorkPeriodsFormProvider
import javax.inject.Inject
import models.{NormalMode, Period, UserAnswers}
import navigation.Navigator
import pages.{ClaimPeriodPage, LastPayDatePage, PayFrequencyPage, SelectWorkPeriodsPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import repositories.SessionRepository
import services.PeriodHelper
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.SelectWorkPeriodsView

import scala.concurrent.{ExecutionContext, Future}

class SelectWorkPeriodsController @Inject()(
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: SelectWorkPeriodsFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: SelectWorkPeriodsView
)(implicit ec: ExecutionContext)
    extends FrontendBaseController with I18nSupport with PeriodHelper {

  private val form = formProvider()

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val preparedForm = request.userAnswers.get(SelectWorkPeriodsPage) match {
      case None       => form
      case Some(list) => form.fill(list)
    }
    getView(request.userAnswers, periods => Ok(view(preparedForm, periods)))
  }

  def onSubmit(): Action[AnyContent] = (getSession andThen getData andThen requireData).async { implicit request =>
    form
      .bindFromRequest()
      .fold(
        formWithErrors => {
          Future.successful(getView(request.userAnswers, periods => BadRequest(view(formWithErrors, periods))))
        },
        value =>
          for {
            updatedAnswers <- Future.fromTry(request.userAnswers.set(SelectWorkPeriodsPage, value))
            _              <- sessionRepository.set(updatedAnswers)
          } yield Redirect(navigator.nextPage(SelectWorkPeriodsPage, NormalMode, updatedAnswers))
      )
  }

  private def getView(userAnswers: UserAnswers, result: (List[Period]) => Result) = {
    val maybeClaimPeriod = userAnswers.get(ClaimPeriodPage)
    val maybePayFrequency = userAnswers.get(PayFrequencyPage)
    val maybeLastPayDay = userAnswers.get(LastPayDatePage)

    (maybeClaimPeriod, maybePayFrequency, maybeLastPayDay) match {
      case (Some(cp), Some(pf), Some(lpd)) => result(getPayPeriods(lpd, pf, cp.supportClaimPeriod))
      case (None, _, _)                    => Redirect(routes.ClaimPeriodController.onPageLoad())
      case (_, None, _)                    => Redirect(routes.PayFrequencyController.onPageLoad())
      case (_, _, None)                    => Redirect(routes.LastPayDateController.onPageLoad())
    }
  }
}
