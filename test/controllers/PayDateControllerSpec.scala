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

import java.time.{LocalDate, ZoneOffset}

import base.SpecBase
import forms.PayDateFormProvider
import models.UserAnswers
import navigation.{FakeNavigator, Navigator}
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.{ClaimPeriodPage, PayDatePage}
import play.api.inject.bind
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded, Call}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.SessionRepository
import views.html.PayDateView

import scala.concurrent.Future

class PayDateControllerSpec extends SpecBase with MockitoSugar {

  private val formProvider = new PayDateFormProvider()
  private def form = formProvider()

  private def onwardRoute = Call("GET", "/foo")

  private val validAnswer = LocalDate.now(ZoneOffset.UTC)

  private lazy val payDateRoute = routes.PayDateController.onPageLoad().url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val getRequest: FakeRequest[AnyContentAsEmpty.type] =
    fakeRequest(GET, payDateRoute)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, payDateRoute)
      .withFormUrlEncodedBody(
        "value.day"   -> validAnswer.getDayOfMonth.toString,
        "value.month" -> validAnswer.getMonthValue.toString,
        "value.year"  -> validAnswer.getYear.toString
      )

  val claimPeriod = LocalDate.of(2020, 11, 1)

  val userAnswers = UserAnswers(userAnswersId, Json.obj(ClaimPeriodPage.toString -> JsString("November 2020")))

  "PayDate Controller" must {

    "return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val result = route(application, getRequest).value

        val view = application.injector.instanceOf[PayDateView]

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form, claimPeriod)(getRequest, messages(application)).toString
      }
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers =
        UserAnswers(userAnswersId, Json.obj(PayDatePage.toString -> validAnswer, ClaimPeriodPage.toString -> JsString("November 2020")))

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val view = application.injector.instanceOf[PayDateView]

        val result = route(application, getRequest).value

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form.fill(validAnswer), claimPeriod)(getRequest, messages(application)).toString
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

        val result = route(application, postRequest).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request =
          fakeRequest(POST, payDateRoute)
            .withFormUrlEncodedBody(("value", "invalid value"))

        val boundForm = form.bind(Map("value" -> "invalid value"))

        val view = application.injector.instanceOf[PayDateView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST

        contentAsString(result) mustEqual
          view(boundForm, claimPeriod)(request, messages(application)).toString
      }
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val result = route(application, getRequest).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val result = route(application, postRequest).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }
  }
}
