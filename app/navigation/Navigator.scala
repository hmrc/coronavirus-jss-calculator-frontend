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

package navigation

import controllers.routes
import javax.inject.{Inject, Singleton}
import models.PayFrequency.Monthly
import models.PayMethod.{Regular, Variable}
import models.{UserAnswers, _}
import pages._
import play.api.mvc.Call

@Singleton
class Navigator @Inject()() {

  private val normalRoutes: Page => UserAnswers => Call = {
    case ClaimPeriodPage =>
      _ =>
        routes.PayFrequencyController.onPageLoad()
    case PayFrequencyPage =>
      _ =>
        routes.LastPayDateController.onPageLoad()
    case PayMethodPage =>
      userAnswers =>
        payMethodRoutes(userAnswers)
    case LastPayDatePage =>
      userAnswers =>
        lastPayDateRoute(userAnswers)
    case EndPayDatePage =>
      _ =>
        routes.PayMethodController.onPageLoad()
    case PayPeriodsPage =>
      userAnswers =>
        payPeriodsRoute(userAnswers)
    case SelectWorkPeriodsPage =>
      _ =>
        routes.RegularPayAmountController.onPageLoad()

    case RegularPayAmountPage =>
      _ =>
        routes.TemporaryWorkingAgreementController.onPageLoad()
    case TemporaryWorkingAgreementPage =>
      _ =>
        routes.BusinessClosedController.onPageLoad()
    case BusinessClosedPage =>
      _ =>
        routes.UsualAndActualHoursController.onPageLoad(1)
    case _ =>
      _ =>
        routes.StartPageController.onPageLoad()
  }

  def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers, idx: Option[Int] = None): Call =
    idx.fold(normalRoutes(page)(userAnswers))(idx => idxRoutes(page)(idx, userAnswers))

  private val idxRoutes: Page => (Int, UserAnswers) => Call = {
    case UsualAndActualHoursPage => selectUsualAndActualHoursPageRoutes
    case _ =>
      (_, _) =>
        routes.StartPageController.onPageLoad()
  }

  private def payPeriodsRoute(userAnswers: UserAnswers): Call =
    userAnswers.get(PayPeriodsPage) match {
      case Some(PayPeriods.Yes) => routes.SelectWorkPeriodsController.onPageLoad()
      case Some(PayPeriods.No)  => routes.LastPayDateController.onPageLoad()
      case _                    => routes.LastPayDateController.onPageLoad()
    }

  private def lastPayDateRoute(userAnswers: UserAnswers): Call =
    userAnswers.get(PayFrequencyPage) match {
      case Some(Monthly) => routes.EndPayDateController.onPageLoad()
      case Some(_)       => routes.PayMethodController.onPageLoad()
      case _             => routes.LastPayDateController.onPageLoad()
    }

  private def selectUsualAndActualHoursPageRoutes: (Int, UserAnswers) => Call = { (previousIdx, userAnswers) =>
    userAnswers.get(SelectWorkPeriodsPage) match {
      case Some(workPeriods) if workPeriods.isDefinedAt(previousIdx) =>
        routes.UsualAndActualHoursController.onPageLoad(previousIdx + 1)
      case Some(_) => routes.ConfirmationController.onPageLoad()
      case _       => routes.SelectWorkPeriodsController.onPageLoad()
    }
  }

  private def payMethodRoutes(userAnswers: UserAnswers): Call =
    userAnswers.get(PayMethodPage) match {
      case Some(Regular)  => routes.PayPeriodsController.onPageLoad()
      case Some(Variable) => routes.ComingSoonController.onPageLoad()
      case _              => routes.PayMethodController.onPageLoad()
    }
}
