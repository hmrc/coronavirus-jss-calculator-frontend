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
import models.BusinessClosed._
import models.PayFrequency.Monthly
import models.PayMethod.{Regular, Variable}
import models.{UserAnswers, _}
import pages._
import play.api.mvc.Call

@Singleton
class Navigator @Inject() () {

  private val normalRoutes: Page => UserAnswers => Call = {
    case ClaimPeriodPage       =>
      _ => routes.PayFrequencyController.onPageLoad()
    case PayFrequencyPage      =>
      _ => routes.LastPayDateController.onPageLoad()
    case PayMethodPage         =>
      userAnswers => payMethodRoutes(userAnswers)
    case LastPayDatePage       =>
      userAnswers => lastPayDateRoute(userAnswers)
    case EndPayDatePage        =>
      _ => routes.PayMethodController.onPageLoad()
    case PayPeriodsPage        =>
      userAnswers => payPeriodsRoute(userAnswers)
    case SelectWorkPeriodsPage =>
      _ => routes.RegularPayAmountController.onPageLoad()

    case RegularPayAmountPage          =>
      _ => routes.TemporaryWorkingAgreementController.onPageLoad()
    case TemporaryWorkingAgreementPage =>
      userAnswers => temporaryWorkingAgreementRoutes(userAnswers)
    case BusinessClosedPage            =>
      userAnswers => businessClosedRoutes(userAnswers)
    case _                             =>
      _ => routes.StartPageController.onPageLoad()
  }

  def nextPage(page: Page, mode: Mode, userAnswers: UserAnswers, idx: Option[Int] = None): Call =
    idx.fold(normalRoutes(page)(userAnswers))(idx => idxRoutes(page)(idx, userAnswers))

  private val idxRoutes: Page => (Int, UserAnswers) => Call = {
    case UsualAndActualHoursPage             => selectUsualAndActualHoursPageRoutes
    case ShortTermWorkingAgreementPeriodPage => shortTermWorkingAgreementPeriodRoutes
    case BusinessClosedPeriodsPage           =>
      (_, _) => routes.CheckYourBusinessClosedPeriodsController.onPageLoad()
    case _                                   =>
      (_, _) => routes.StartPageController.onPageLoad()
  }

  private def shortTermWorkingAgreementPeriodRoutes(idx: Int, answers: UserAnswers): Call =
    answers.get(ShortTermWorkingAgreementPeriodPage, Some(idx)) match {
      case Some(answer) if answer.addAnother => routes.ShortTermWorkingAgreementPeriodController.onPageLoad(idx + 1)
      case Some(_)                           => routes.BusinessClosedController.onPageLoad()
      case None                              => routes.StartPageController.onPageLoad()
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
      case Some(_)                                                   => routes.ConfirmationController.onPageLoad()
      case _                                                         => routes.SelectWorkPeriodsController.onPageLoad()
    }
  }

  private def payMethodRoutes(userAnswers: UserAnswers): Call =
    userAnswers.get(PayMethodPage) match {
      case Some(Regular)  => routes.PayPeriodsController.onPageLoad()
      case Some(Variable) => routes.ComingSoonController.onPageLoad()
      case _              => routes.PayMethodController.onPageLoad()
    }

  private def temporaryWorkingAgreementRoutes(userAnswers: UserAnswers): Call =
    userAnswers.get(TemporaryWorkingAgreementPage) match {
      case Some(TemporaryWorkingAgreement.Yes) => routes.ShortTermWorkingAgreementPeriodController.onPageLoad(1)
      case Some(TemporaryWorkingAgreement.No)  => routes.BusinessClosedController.onPageLoad()
      case _                                   => routes.TemporaryWorkingAgreementController.onPageLoad()
    }

  private def businessClosedRoutes(userAnswers: UserAnswers): Call =
    (userAnswers.get(BusinessClosedPage), userAnswers.get(TemporaryWorkingAgreementPage)) match {
      case (Some(BusinessClosed.Yes), _)                                  => routes.BusinessClosedPeriodsController.onPageLoad(1)
      case (Some(BusinessClosed.No), Some(TemporaryWorkingAgreement.Yes)) =>
        routes.UsualAndActualHoursController.onPageLoad(1)
      case (Some(BusinessClosed.No), Some(TemporaryWorkingAgreement.No))  =>
        routes.YouAreNotEligibleController.onPageLoad()
      case _                                                              => routes.BusinessClosedController.onPageLoad()
    }
}
