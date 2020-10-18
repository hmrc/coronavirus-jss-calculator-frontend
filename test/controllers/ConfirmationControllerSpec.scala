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

import base.SpecBase
import models.PayFrequency.Weekly
import models.{Amount, ClaimPeriod, Grant, GrantForPeriod, Period, PeriodWithHours, UsualAndActualHours}
import pages._
import play.api.test.Helpers._
import views.html.ConfirmationView
import java.time.LocalDate

class ConfirmationControllerSpec extends SpecBase {

  "Confirmation Controller" must {

    val start = LocalDate.of(2020, 10, 20)
    val end = LocalDate.of(2020, 11, 20)
    val usualHours = 10.00
    val actualHours = 5.00
    val regularPay = 1234.00

    val periods = List(Period(start, end))

    val userAnswers = emptyUserAnswers
      .set(ClaimPeriodPage, ClaimPeriod.Nov2020)
      .success
      .value
      .set(SelectWorkPeriodsPage, periods)
      .success
      .value
      .set(PayFrequencyPage, Weekly)
      .success
      .value
      .set(RegularPayAmountPage, Amount(regularPay))
      .success
      .value
      .setList(UsualAndActualHoursPage, List(UsualAndActualHours(usualHours, actualHours)))
      .success
      .value

    val periodWithHours = PeriodWithHours(start, end, usualHours, actualHours)
    val grantForPeriod = GrantForPeriod(periodWithHours, 138.89, 10, 10, 200.00, 120.22, 23.22, false)

    val grant = Grant(List(grantForPeriod), true, 138.89)

    "return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request = fakeRequest(GET, routes.ConfirmationController.onPageLoad().url)

        val result = route(application, request).value

        val view = application.injector.instanceOf[ConfirmationView]

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(grant, regularPay, "1.0")(request, messages(application)).toString
      }
    }
  }
}
