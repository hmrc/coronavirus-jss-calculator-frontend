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

import base.SpecBaseControllerSpecs

class ConfirmationControllerSpec extends SpecBaseControllerSpecs {

//  val view = app.injector.instanceOf[ConfirmationView]

//  private lazy val confirmationRoute = routes.ConfirmationController.onPageLoad().url

  "Confirmation Controller" must {

//    val start       = LocalDate.of(2020, 10, 26)
//    val end         = LocalDate.of(2020, 11, 24)
//    val usualHours  = 20.00
//    val actualHours = 10.00
//    val regularPay  = 1234.00
//
//    val periods = List(Period(start, end))

//    val userAnswers = emptyUserAnswers
//      .set(ClaimPeriodPage, ClaimPeriod.Nov2020)
//      .success
//      .value
//      .set(SelectWorkPeriodsPage, periods)
//      .success
//      .value
//      .set(PayFrequencyPage, Monthly)
//      .success
//      .value
//      .set(RegularPayAmountPage, Amount(regularPay))
//      .success
//      .value
//      .setList(UsualAndActualHoursPage, List(UsualAndActualHours(usualHours, actualHours)))
//      .success
//      .value
//
//    val jobSupport = JobSupport(
//      List(
//        PeriodSupport(
//          LocalDate.now.minusDays(2),
//          LocalDate.now.plusDays(1),
//          10,
//          JobSupportOpen.noSupport,
//          JobSupportClosed.noSupport
//        )
//      ),
//      200
//    )

    //TODO: disabled test for now until design and content is fixed.
//    "return OK and the correct view for a GET" in {
//
//      val controller = new ConfirmationController(
//        messagesApi,
//        getSessionAction,
//        stubDataRetrieval(Some(userAnswers)),
//        dataRequired,
//        component,
//        view,
//        frontendAppConfig,
//        audit
//      )
//
//      val fakeRequest =
//        FakeRequest("GET", confirmationRoute).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]
//
//      val result = controller.onPageLoad()(fakeRequest)
//
//      status(result) mustEqual OK
//
//      contentAsString(result) mustEqual
//        view(jobSupport, "1.0")(fakeRequest, messages).toString
//    }
  }
}
