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
import forms.BusinessClosedPeriodsFormProvider
import javax.inject.Inject
import models.{BusinessClosedPeriod, NormalMode, SupportClaimPeriod, UserAnswers}
import navigation.Navigator
import pages.{BusinessClosedPeriodsPage, ClaimPeriodPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import repositories.SessionRepository
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.BusinessClosedPeriodsView

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class BusinessClosedPeriodsController @Inject() (
  override val messagesApi: MessagesApi,
  sessionRepository: SessionRepository,
  navigator: Navigator,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  formProvider: BusinessClosedPeriodsFormProvider,
  val controllerComponents: MessagesControllerComponents,
  view: BusinessClosedPeriodsView,
  config: FrontendAppConfig
)(implicit ec: ExecutionContext)
    extends FrontendBaseController
    with I18nSupport {

  private def form(previousBCPeriods: Seq[BusinessClosedPeriod], claimPeriod: SupportClaimPeriod) =
    formProvider(previousBCPeriods)

  def onPageLoad(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val previousBCPeriods =
      request.userAnswers.getList(BusinessClosedPeriodsPage)

    request.userAnswers.get(ClaimPeriodPage).map(_.supportClaimPeriod) match {
      case Some(cp) =>
        val preparedForm = request.userAnswers.get(BusinessClosedPeriodsPage, Some(idx)) match {
          case None        => form(previousBCPeriods, cp)
          case Some(value) => form(previousBCPeriods, cp).fill(value)
        }

        Ok(view(preparedForm, idx, config.maxClosedPeriods))
      case None     => Redirect(routes.ClaimPeriodController.onPageLoad())
    }
  }

  def onSubmit(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      val previousBCPeriods =
        request.userAnswers.getList(BusinessClosedPeriodsPage).zipWithIndex.filter(_._2 != idx - 1).map(_._1)

      request.userAnswers.get(ClaimPeriodPage).map(_.supportClaimPeriod) match {
        case Some(cp) =>
          form(previousBCPeriods, cp)
            .bindFromRequest()
            .fold(
              formWithErrors => Future.successful(BadRequest(view(formWithErrors, idx, config.maxClosedPeriods))),
              value => {
                var updatedAnswers = request.userAnswers.set(BusinessClosedPeriodsPage, value, Some(idx))
                if (!value.addAnother) {
                  updatedAnswers = trimListWhenUserSaysNoToAddMore(updatedAnswers, idx)
                }
                for {
                  updatedAnswers <- Future.fromTry(updatedAnswers)
                  _              <- sessionRepository.set(updatedAnswers)
                } yield Redirect(navigator.nextPage(BusinessClosedPeriodsPage, NormalMode, updatedAnswers, Some(idx)))
              }
            )
        case None     => Future.successful(Redirect(routes.ClaimPeriodController.onPageLoad()))
      }
  }

  def remove(idx: Int): Action[AnyContent] = (getSession andThen getData andThen requireData).async {
    implicit request =>
      val existingPeriods  = request.userAnswers.getList(BusinessClosedPeriodsPage)
      val remainingPeriods = existingPeriods.zipWithIndex.filterNot(p => p._2 == idx).map(_._1)

      for {
        updatedAnswers <- Future.fromTry(request.userAnswers.setList(BusinessClosedPeriodsPage, remainingPeriods))
        _              <- sessionRepository.set(updatedAnswers)
      } yield Redirect(navigator.nextPage(BusinessClosedPeriodsPage, NormalMode, updatedAnswers, Some(idx)))

  }

  private def trimListWhenUserSaysNoToAddMore(userAnswers: Try[UserAnswers], idx: Int) =
    userAnswers.flatMap { ua =>
      val trimmedList = ua.getList(BusinessClosedPeriodsPage).slice(0, idx)
      ua.setList(BusinessClosedPeriodsPage, trimmedList)
    }
}
