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

import base.SpecBase
import forms.PayPeriodsFormProvider
import models.{ClaimPeriod, PayFrequency, PayPeriods, Period, UserAnswers}
import navigation.{FakeNavigator, Navigator}
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.{ClaimPeriodPage, LastPayDatePage, PayFrequencyPage, PayPeriodsPage}
import play.api.inject.bind
import play.api.libs.json.{JsString, Json}
import play.api.mvc.Call
import play.api.test.Helpers._
import repositories.SessionRepository
import views.html.PayPeriodsView

import scala.concurrent.Future

class PayPeriodsControllerSpec extends SpecBase with MockitoSugar {

  private def onwardRoute = Call("GET", "/foo")

  private lazy val payPeriodsRoute = routes.PayPeriodsController.onPageLoad().url

  private val formProvider = new PayPeriodsFormProvider()
  private val form = formProvider()

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
  val periods = List(Period(LocalDate.of(2020, 10, 31), LocalDate.of(2020, 11, 29)))

  "PayPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request = fakeRequest(GET, payPeriodsRoute)

        val result = route(application, request).value

        val view = application.injector.instanceOf[PayPeriodsView]

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form, periods, claimPeriod.yearMonth)(request, messages(application)).toString
      }
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(PayPeriodsPage, PayPeriods.values.head).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswersUpdated)).build()

      running(application) {

        val request = fakeRequest(GET, payPeriodsRoute)

        val view = application.injector.instanceOf[PayPeriodsView]

        val result = route(application, request).value

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form.fill(PayPeriods.values.head), periods, claimPeriod.yearMonth)(request, messages(application)).toString
      }
    }

    "redirect to the next page when valid data is submitted" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {

        val request =
          fakeRequest(POST, payPeriodsRoute)
            .withFormUrlEncodedBody(("value", PayPeriods.values.head.toString))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request =
          fakeRequest(POST, payPeriodsRoute)
            .withFormUrlEncodedBody(("value", "invalid value"))

        val boundForm = form.bind(Map("value" -> "invalid value"))

        val view = application.injector.instanceOf[PayPeriodsView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST

        contentAsString(result) mustEqual
          view(boundForm, periods, claimPeriod.yearMonth)(request, messages(application)).toString
      }
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val request = fakeRequest(GET, payPeriodsRoute)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val request =
          fakeRequest(POST, payPeriodsRoute)
            .withFormUrlEncodedBody(("value", PayPeriods.values.head.toString))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }
  }
}
