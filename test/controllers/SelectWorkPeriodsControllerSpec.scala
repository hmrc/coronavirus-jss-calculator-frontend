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

import java.time.LocalDate

import base.SpecBaseControllerSpecs
import forms.SelectWorkPeriodsFormProvider
import models.PayFrequency.Monthly
import models.{ClaimPeriod, PayFrequency, Period, UserAnswers}
import pages.{ClaimPeriodPage, EndPayDatePage, LastPayDatePage, PayFrequencyPage, SelectWorkPeriodsPage}
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import views.html.SelectWorkPeriodsView

import scala.concurrent.ExecutionContext.Implicits.global

class SelectWorkPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[SelectWorkPeriodsView]

  private lazy val selectWorkPeriodsRouteGet  = routes.SelectWorkPeriodsController.onPageLoad().url
  private lazy val selectWorkPeriodsRoutePost = routes.SelectWorkPeriodsController.onSubmit().url

  private val formProvider = new SelectWorkPeriodsFormProvider()
  private val form         = formProvider()

  val claimPeriod  = ClaimPeriod.Nov2020
  val payFrequency = PayFrequency.FortNightly
  val lastPayDate  = "2020-10-31"
  val endPayDate   = "2020-11-30"

  val userAnswers = UserAnswers(
    userAnswersId,
    Json.obj(
      ClaimPeriodPage.toString  -> JsString(claimPeriod),
      PayFrequencyPage.toString -> JsString(payFrequency),
      LastPayDatePage.toString  -> JsString(lastPayDate),
      EndPayDatePage.toString   -> JsString(endPayDate)
    )
  )

  val period1 = Period(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 14))
  val period2 = Period(LocalDate.of(2020, 11, 15), LocalDate.of(2020, 11, 28))

  val periods = List(period1, period2)

  def controller(userAnswers: Option[UserAnswers]) = new SelectWorkPeriodsController(
    messagesApi,
    mockSessionRepository,
    navigator,
    getSessionAction,
    stubDataRetrieval(userAnswers),
    dataRequired,
    formProvider,
    component,
    view
  )

  "SelectWorkPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, selectWorkPeriodsRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, periods)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(SelectWorkPeriodsPage, periods).success.value

      val request = fakeRequest(GET, selectWorkPeriodsRouteGet)

      val result = controller(Some(userAnswersUpdated)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(periods), periods)(request, messages).toString
    }

    "redirect to /regular-pay-amount when only one period exist for a GET" in {

      val request = fakeRequest(GET, selectWorkPeriodsRouteGet)

      val result = controller(Some(userAnswers.set(PayFrequencyPage, Monthly).success.value)).onPageLoad()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.RegularPayAmountController.onPageLoad().url
    }

    "redirect to the next page when valid data is submitted" in {

      val request =
        fakeRequest(POST, selectWorkPeriodsRoutePost)
          .withFormUrlEncodedBody(("value[0]", LocalDate.now().toString + "_" + LocalDate.now().plusDays(1).toString))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.RegularPayAmountController.onPageLoad().url
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val request =
        fakeRequest(POST, selectWorkPeriodsRoutePost)
          .withFormUrlEncodedBody(("value", "invalid value"))

      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, periods)(request, messages).toString
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val request = fakeRequest(GET, selectWorkPeriodsRouteGet)

      val result = controller(None).onPageLoad()(request)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val request =
        fakeRequest(POST, selectWorkPeriodsRoutePost)
          .withFormUrlEncodedBody(("value[0]", LocalDate.now().toString))

      val result = controller(None).onSubmit()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }
  }
}
