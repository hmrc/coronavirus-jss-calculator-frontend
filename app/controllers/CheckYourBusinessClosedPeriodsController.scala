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
import javax.inject.Inject
import models.{BusinessClosed, TemporaryWorkingAgreement}
import pages.{BusinessClosedPage, BusinessClosedPeriodsPage, TemporaryWorkingAgreementPage}
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.play.bootstrap.controller.FrontendBaseController
import views.html.CheckYourBusinessClosedPeriodsView

class CheckYourBusinessClosedPeriodsController @Inject()(
  override val messagesApi: MessagesApi,
  getSession: GetSessionAction,
  getData: DataRetrievalAction,
  requireData: DataRequiredAction,
  val controllerComponents: MessagesControllerComponents,
  view: CheckYourBusinessClosedPeriodsView
) extends FrontendBaseController with I18nSupport {

  def onPageLoad(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    Ok(view(request.userAnswers.getList(BusinessClosedPeriodsPage)))
  }

  def confirm(): Action[AnyContent] = (getSession andThen getData andThen requireData) { implicit request =>
    val stwa = request.userAnswers.get(TemporaryWorkingAgreementPage)
    val businessClosed = request.userAnswers.get(BusinessClosedPage)

    val result = (stwa, businessClosed) match {
      case (Some(TemporaryWorkingAgreement.Yes), _)                       => routes.UsualAndActualHoursController.onPageLoad(1)
      case (Some(TemporaryWorkingAgreement.No), Some(BusinessClosed.Yes)) => routes.ConfirmationController.onPageLoad()
      case (Some(TemporaryWorkingAgreement.No), Some(BusinessClosed.No))  => routes.YouAreNotEligibleController.onPageLoad()
      case (None, _)                                                      => routes.TemporaryWorkingAgreementController.onPageLoad()
      case (_, None)                                                      => routes.BusinessClosedController.onPageLoad()
    }

    Redirect(result)
  }
}
