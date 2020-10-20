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
import forms.PayPeriodsFormProvider
import models.{ClaimPeriod, PayFrequency, PayPeriods, Period, UserAnswers}
import pages.{ClaimPeriodPage, LastPayDatePage, PayFrequencyPage, PayPeriodsPage}
import play.api.libs.json.{JsString, Json}
import play.api.test.Helpers._
import views.html.PayPeriodsView

import scala.concurrent.ExecutionContext.Implicits.global

class PayPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[PayPeriodsView]
  private lazy val payPeriodsRouteGet = routes.PayPeriodsController.onPageLoad().url
  private lazy val payPeriodsRoutePost = routes.PayPeriodsController.onSubmit().url

  private val formProvider = new PayPeriodsFormProvider()
  private val form = formProvider()

  def controller(userAnswers: Option[UserAnswers]) = new PayPeriodsController(
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

  val claimPeriod = ClaimPeriod.Nov2020
  val payFrequency = PayFrequency.Monthly
  val lastPayDate = "2020-10-30"

  val userAnswers = UserAnswers(
    userAnswersId,
    Json.obj(
      ClaimPeriodPage.toString  -> JsString(claimPeriod),
      PayFrequencyPage.toString -> JsString(payFrequency),
      LastPayDatePage.toString  -> JsString(lastPayDate))
  )
  val periods = List(Period(LocalDate.of(2020, 10, 31), LocalDate.of(2020, 11, 30)))

  "PayPeriods Controller" must {

    "return OK and the correct view for a GET" in {
      val request = fakeRequest(GET, payPeriodsRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, periods)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(PayPeriodsPage, PayPeriods.values.head).success.value

      val request = fakeRequest(GET, payPeriodsRouteGet)

      val result = controller(Some(userAnswersUpdated)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(PayPeriods.values.head), periods)(request, messages).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val request =
        fakeRequest(POST, payPeriodsRoutePost)
          .withFormUrlEncodedBody(("value", PayPeriods.values.head.toString))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SelectWorkPeriodsController.onPageLoad().url
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val request =
        fakeRequest(POST, payPeriodsRoutePost)
          .withFormUrlEncodedBody(("value", "invalid value"))

      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, periods)(request, messages).toString
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val request = fakeRequest(GET, payPeriodsRouteGet)

      val result = controller(None).onPageLoad()(request)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val request =
        fakeRequest(POST, payPeriodsRoutePost)
          .withFormUrlEncodedBody(("value", PayPeriods.values.head.toString))

      val result = controller(None).onSubmit()(request)
      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }
  }
}
