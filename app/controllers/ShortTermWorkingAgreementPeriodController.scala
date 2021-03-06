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
import forms.ShortTermWorkingAgreementPeriodFormProvider
import javax.inject.Inject
import models.{TemporaryWorkingAgreementPeriod, UserAnswers}
import navigation.Navigator
import pages.ShortTermWorkingAgreementPeriodPage
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.ShortTermWorkingAgreementPeriodView

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class ShortTermWorkingAgreementPeriodController @Inject() (
  override val messagesApi: MessagesApi,
  val sessionRepository: SessionRepository,
  val navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: ShortTermWorkingAgreementPeriodFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: ShortTermWorkingAgreementPeriodView,
  config: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport
    with ControllerHelper {

  private def form(previousTWAPeriod: Seq[TemporaryWorkingAgreementPeriod]) =
    formProvider(previousTWAPeriod)

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val previousTWAPeriods = request.userAnswers.getList(ShortTermWorkingAgreementPeriodPage)

    val preparedForm = request.userAnswers.get(ShortTermWorkingAgreementPeriodPage, Some(idx)) match {
      case None        => form(previousTWAPeriods)
      case Some(value) => form(previousTWAPeriods).fill(value)
    }

    Ok(view(preparedForm, idx, config.maxStwaPeriods))
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      val previousTWAPeriods =
        request.userAnswers.getList(ShortTermWorkingAgreementPeriodPage).zipWithIndex.filter(_._2 < idx - 1).map(_._1)
      form(previousTWAPeriods)
        .bindFromRequest()
        .fold(
          formWithErrors => Future.successful(BadRequest(view(formWithErrors, idx, config.maxStwaPeriods))),
          value => {
            var updatedAnswers = request.userAnswers.set(ShortTermWorkingAgreementPeriodPage, value, Some(idx))
            updatedAnswers = invalidateList(updatedAnswers, idx)
            saveAndRedirect(ShortTermWorkingAgreementPeriodPage, updatedAnswers, Some(idx))
          }
        )
  }

  private def invalidateList(userAnswers: Try[UserAnswers], idx: Int) =
    //deletes the list elements after 'idx'
    userAnswers.flatMap { ua =>
      val trimmedList = ua.getList(ShortTermWorkingAgreementPeriodPage).slice(0, idx)
      ua.setList(ShortTermWorkingAgreementPeriodPage, trimmedList)
    }
}
