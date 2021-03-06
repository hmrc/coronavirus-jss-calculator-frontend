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
import models.PayMethod.{Regular, Variable}
import models._
import pages._

class NavigatorSpec extends SpecBase {

  val navigator = new Navigator

  "Navigator" when {

    "in Normal mode" must {

      val periods = List(Period(LocalDate.now().minusDays(10), LocalDate.now()))

      "go to Index from a page that doesn't exist in the route map" in {
        case object UnknownPage extends Page
        navigator.nextPage(UnknownPage, NormalMode, emptyUserAnswers) mustBe routes.StartPageController.onPageLoad()
      }

      "go to PayFrequencyPage after ClaimPeriod" in {
        navigator.nextPage(ClaimPeriodPage, NormalMode, emptyUserAnswers) mustBe routes.PayFrequencyController
          .onPageLoad()
      }

      "go to LastPayDatePage after PayFrequency" in {
        navigator.nextPage(PayFrequencyPage, NormalMode, emptyUserAnswers) mustBe routes.LastPayDateController
          .onPageLoad()
      }

      "go to correct page after PayMethod" in {
        var userAnswers = emptyUserAnswers.set(PayMethodPage, Regular).success.value
        navigator.nextPage(PayMethodPage, NormalMode, userAnswers) mustBe routes.PayPeriodsController.onPageLoad()

        userAnswers = emptyUserAnswers.set(PayMethodPage, Variable).success.value
        navigator.nextPage(PayMethodPage, NormalMode, userAnswers) mustBe routes.ComingSoonController.onPageLoad()
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
        navigator.nextPage(PayPeriodsPage, NormalMode, userAnswers) mustBe routes.SelectWorkPeriodsController
          .onPageLoad()

        userAnswers = emptyUserAnswers.set(PayPeriodsPage, PayPeriods.No).success.value
        navigator.nextPage(PayPeriodsPage, NormalMode, userAnswers) mustBe routes.LastPayDateController.onPageLoad()
      }

      "go to correct page after SelectWorkPeriodsPage" in {
        val userAnswers = emptyUserAnswers.set(SelectWorkPeriodsPage, periods).success.value
        navigator.nextPage(SelectWorkPeriodsPage, NormalMode, userAnswers) mustBe routes.RegularPayAmountController
          .onPageLoad()
      }

      "go to TemporaryWorkingAgreementPage after RegularPayAmountPage" in {
        val userAnswers = emptyUserAnswers.set(SelectWorkPeriodsPage, periods).success.value
        navigator.nextPage(
          RegularPayAmountPage,
          NormalMode,
          userAnswers
        ) mustBe routes.TemporaryWorkingAgreementController.onPageLoad()
      }

      "go from ShortTermWorkingAgreementPeriodPage to ShortTermWorkingAgreementPeriodPage for the next index" when {

        "the user selects Yes on `addAnother`" in {
          val userAnswers = emptyUserAnswers
            .set(
              ShortTermWorkingAgreementPeriodPage,
              TemporaryWorkingAgreementPeriod(LocalDate.now, LocalDate.now, addAnother = true),
              Some(1)
            )
            .success
            .value
          navigator
            .nextPage(
              ShortTermWorkingAgreementPeriodPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.ShortTermWorkingAgreementPeriodController
            .onPageLoad(2)
        }
      }

      "go from ShortTermWorkingAgreementPeriodPage to BusinessClosedPage" when {

        "the user select No on `addAnother`" in {
          val userAnswers = emptyUserAnswers
            .set(
              ShortTermWorkingAgreementPeriodPage,
              TemporaryWorkingAgreementPeriod(LocalDate.now, LocalDate.now, addAnother = false),
              Some(1)
            )
            .success
            .value
          navigator
            .nextPage(
              ShortTermWorkingAgreementPeriodPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.BusinessClosedController
            .onPageLoad()
        }
      }

      "go to BusinessClosedPage after TemporaryWorkingAgreementPage" in {
        val userAnswers =
          emptyUserAnswers.set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.Yes).success.value
        navigator.nextPage(
          TemporaryWorkingAgreementPage,
          NormalMode,
          userAnswers
        ) mustBe routes.ShortTermWorkingAgreementPeriodController
          .onPageLoad(1)
      }

      "go to BusinessClosedPeriodsPage after BusinessClosedPage if user says yes" in {
        val userAnswers = emptyUserAnswers.set(BusinessClosedPage, BusinessClosed.Yes).success.value
        navigator.nextPage(BusinessClosedPage, NormalMode, userAnswers) mustBe routes.BusinessClosedPeriodsController
          .onPageLoad(1)
      }

      "go from BusinessClosedPeriodsPage to BusinessClosedPeriodsPage for the next index" when {

        "the user answers Yes to adding another period" in {
          val userAnswers = emptyUserAnswers
            .set(
              BusinessClosedPeriodsPage,
              BusinessClosedPeriod(LocalDate.now, LocalDate.now, addAnother = true),
              Some(1)
            )
            .success
            .value

          navigator
            .nextPage(
              BusinessClosedPeriodsPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.BusinessClosedPeriodsController
            .onPageLoad(2)
        }
      }

      "go from BusinessClosedPeriodsPage to UsualAndActualHoursPage at index 1" when {

        "the user answers No to adding more periods, and there is a STWA in place" in {
          val userAnswers = emptyUserAnswers
            .set(
              BusinessClosedPeriodsPage,
              BusinessClosedPeriod(LocalDate.now, LocalDate.now, addAnother = false),
              Some(1)
            )
            .success
            .value
            .set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.Yes, None)
            .success
            .value

          navigator
            .nextPage(
              BusinessClosedPeriodsPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.UsualAndActualHoursController
            .onPageLoad(1)
        }
      }

      "go from BusinessClosedPeriodsPage to Confirmation" when {

        "the user answers No to adding more periods, there is not a STWA in place, and the business closed" in {
          val userAnswers = emptyUserAnswers
            .set(
              BusinessClosedPeriodsPage,
              BusinessClosedPeriod(LocalDate.now, LocalDate.now, addAnother = false),
              Some(1)
            )
            .success
            .value
            .set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.No, None)
            .success
            .value
            .set(BusinessClosedPage, BusinessClosed.Yes, None)
            .success
            .value

          navigator
            .nextPage(
              BusinessClosedPeriodsPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.ConfirmationController.onPageLoad()
        }
      }

      "go from BusinessClosedPeriodsPage to NotEligible" when {

        "the user answers No to adding more periods, there is not a STWA in place, and the business was not closed" in {
          val userAnswers = emptyUserAnswers
            .set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.No, None)
            .success
            .value
            .set(BusinessClosedPage, BusinessClosed.No, None)
            .success
            .value
            .set(
              BusinessClosedPeriodsPage,
              BusinessClosedPeriod(LocalDate.now, LocalDate.now, addAnother = false),
              Some(1)
            )
            .success
            .value

          navigator
            .nextPage(
              BusinessClosedPeriodsPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.YouAreNotEligibleController.onPageLoad()
        }
      }

      "go from BusinessClosedPeriodsPage to Temporary Working Agreement" when {

        "the user answers No to adding more periods and the TWA question has not been answered" in {
          val userAnswers = emptyUserAnswers
            .set(
              BusinessClosedPeriodsPage,
              BusinessClosedPeriod(LocalDate.now, LocalDate.now, addAnother = false),
              Some(1)
            )
            .success
            .value

          navigator
            .nextPage(
              BusinessClosedPeriodsPage,
              NormalMode,
              userAnswers,
              Some(1)
            ) mustBe routes.TemporaryWorkingAgreementController.onPageLoad()
        }
      }

      "go to UsualAndActualHoursPage after BusinessClosedPage if user says no and SWTA is yes" in {
        val userAnswers = emptyUserAnswers
          .set(BusinessClosedPage, BusinessClosed.No)
          .success
          .value
          .set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.Yes)
          .success
          .value
        navigator.nextPage(BusinessClosedPage, NormalMode, userAnswers) mustBe routes.UsualAndActualHoursController
          .onPageLoad(1)
      }

      "go to /you-are not-eligible page after BusinessClosedPage if user says no  and also STWA is also a no" in {
        val userAnswers = emptyUserAnswers
          .set(BusinessClosedPage, BusinessClosed.No)
          .success
          .value
          .set(TemporaryWorkingAgreementPage, TemporaryWorkingAgreement.No)
          .success
          .value
        navigator.nextPage(BusinessClosedPage, NormalMode, userAnswers) mustBe routes.YouAreNotEligibleController
          .onPageLoad()
      }

      "go to Confirmation after UsualAndActualHoursPage" in {
        val userAnswers = emptyUserAnswers
          .set(SelectWorkPeriodsPage, periods)
          .success
          .value
          .set(UsualAndActualHoursPage, UsualAndActualHours(10.00, 20.00), Some(1))
          .success
          .value
        navigator.nextPage(
          UsualAndActualHoursPage,
          NormalMode,
          userAnswers,
          Some(1)
        ) mustBe routes.ConfirmationController.onPageLoad()
      }

      "go to PayMethodPage after EndPayDate" in {
        val userAnswers = emptyUserAnswers.set(EndPayDatePage, LocalDate.now()).success.value
        navigator.nextPage(EndPayDatePage, NormalMode, userAnswers) mustBe routes.PayMethodController.onPageLoad()
      }
    }
  }
}
