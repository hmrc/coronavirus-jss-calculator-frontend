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

import java.time.LocalDate

import base.SpecBase
import controllers.routes
import models.PayFrequency.{Monthly, Weekly}
import models._
import pages._

class NavigatorSpec extends SpecBase {

  val navigator = new Navigator

  "Navigator" when {

    "in Normal mode" must {

      "go to Index from a page that doesn't exist in the route map" in {
        case object UnknownPage extends Page
        navigator.nextPage(UnknownPage, NormalMode, emptyUserAnswers) mustBe routes.StartPageController.onPageLoad()
      }

      "go to PayFrequencyPage after ClaimPeriod" in {
        navigator.nextPage(ClaimPeriodPage, NormalMode, emptyUserAnswers) mustBe routes.PayFrequencyController.onPageLoad()
      }

      "go to LastPayDatePage after PayFrequency" in {
        navigator.nextPage(PayFrequencyPage, NormalMode, emptyUserAnswers) mustBe routes.LastPayDateController.onPageLoad()
      }

      "go to LastPayDatePage after PayMethod" in {
        navigator.nextPage(PayMethodPage, NormalMode, emptyUserAnswers) mustBe routes.PayPeriodsController.onPageLoad()
      }

      "go to correct page after LastPayDatePage" in {
        var userAnswers = emptyUserAnswers.set(LastPayDatePage, LocalDate.now()).success.value
        userAnswers = userAnswers.set(PayFrequencyPage, Monthly).success.value
        navigator.nextPage(LastPayDatePage, NormalMode, userAnswers) mustBe routes.EndPayDateController.onPageLoad()

        userAnswers = userAnswers.set(PayFrequencyPage, Weekly).success.value
        navigator.nextPage(LastPayDatePage, NormalMode, userAnswers) mustBe routes.PayMethodController.onPageLoad()
      }

      "go to correct page after PayPeriodsPage" in {
        var userAnswers = emptyUserAnswers.set(PayPeriodsPage, PayPeriods.Yes).success.value
        navigator.nextPage(PayPeriodsPage, NormalMode, userAnswers) mustBe routes.SelectWorkPeriodsController.onPageLoad()

        userAnswers = emptyUserAnswers.set(PayPeriodsPage, PayPeriods.No).success.value
        navigator.nextPage(PayPeriodsPage, NormalMode, userAnswers) mustBe routes.LastPayDateController.onPageLoad()
      }

      "go to correct page after SelectWorkPeriodsPage" in {
        val userAnswers = emptyUserAnswers.set(SelectWorkPeriodsPage, List(LocalDate.now())).success.value
        navigator.nextPage(SelectWorkPeriodsPage, NormalMode, userAnswers) mustBe routes.RegularPayAmountController.onPageLoad()
      }

      "go to UsualAndActualHoursPage after RegularPayAmountPage" in {
        val userAnswers = emptyUserAnswers.set(SelectWorkPeriodsPage, List(LocalDate.now())).success.value
        navigator.nextPage(RegularPayAmountPage, NormalMode, userAnswers) mustBe routes.UsualAndActualHoursController.onPageLoad(1)
      }

      "go to Confirmation after UsualAndActualHoursPage" in {
        val userAnswers = emptyUserAnswers
          .set(SelectWorkPeriodsPage, List(LocalDate.now()))
          .success
          .value
          .set(UsualAndActualHoursPage, UsualAndActualHours(10.00, 20.00), Some(1))
          .success
          .value
        navigator.nextPage(UsualAndActualHoursPage, NormalMode, userAnswers, Some(1)) mustBe routes.ConfirmationController.onPageLoad()
      }

      "go to PayMethodPage after EndPayDate" in {
        val userAnswers = emptyUserAnswers.set(EndPayDatePage, LocalDate.now()).success.value
        navigator.nextPage(EndPayDatePage, NormalMode, userAnswers) mustBe routes.PayMethodController.onPageLoad()
      }
    }
  }
}
