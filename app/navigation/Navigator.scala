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
import models._
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
        routes.PayMethodController.onPageLoad()
    case PayMethodPage =>
      _ =>
        routes.LastPayDateController.onPageLoad()
    case LastPayDatePage =>
      _ =>
        routes.PayPeriodsController.onPageLoad()
    case PayPeriodsPage =>
      userAnswers =>
        payPeriodsRoute(userAnswers)
    case SelectWorkPeriodsPage =>
      _ =>
        routes.RegularPayAmountController.onPageLoad()

    case RegularPayAmountPage =>
      _ =>
        routes.UsualAndActualHoursController.onPageLoad()
    case _ =>
      _ =>
        routes.StartPageController.onPageLoad()
  }

  def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers): Call = mode match {
    case NormalMode =>
      normalRoutes(page)(userAnswers)
    case m => throw new RuntimeException(s"nextPage not yet implemented in $m")
  }

  private def payPeriodsRoute(userAnswers: UserAnswers): Call =
    userAnswers.get(PayPeriodsPage) match {
      case Some(PayPeriods.Yes) => routes.SelectWorkPeriodsController.onPageLoad()
      case Some(PayPeriods.No)  => routes.LastPayDateController.onPageLoad()
      case _                    => routes.LastPayDateController.onPageLoad()
    }
}
