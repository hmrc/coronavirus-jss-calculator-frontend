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
import forms.UsualAndActualHoursFormProvider
import models.{UserAnswers, UsualAndActualHours}
import navigation.{FakeNavigator, Navigator}
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.{SelectWorkPeriodsPage, UsualAndActualHoursPage}
import play.api.inject.bind
import play.api.mvc.{AnyContentAsEmpty, Call}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.SessionRepository
import views.html.UsualAndActualHoursView

import scala.concurrent.Future

class UsualAndActualHoursControllerSpec extends SpecBase with MockitoSugar {

  private val formProvider = new UsualAndActualHoursFormProvider()
  private val form = formProvider()

  private def onwardRoute = Call("GET", "/foo")

  private val validAnswer = UsualAndActualHours(10.00, 20.00)

  private def usualAndActualHoursRoute(idx: Int) = routes.UsualAndActualHoursController.onPageLoad(idx).url

  def getRequest(method: String, idx: Int) =
    FakeRequest(method, usualAndActualHoursRoute(idx)).withCSRFToken
      .asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  val userAnswers = emptyUserAnswers.set(SelectWorkPeriodsPage, List(LocalDate.now(), LocalDate.now().plusDays(10))).success.value

  "UsualAndActualHours Controller" must {

    "return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request = getRequest(GET, 1)

        val result = route(application, request).value

        val view = application.injector.instanceOf[UsualAndActualHoursView]

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form, 1)(request, messages(application)).toString
      }
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set(UsualAndActualHoursPage, validAnswer, Some(1)).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val request = getRequest("GET", 1)

        val view = application.injector.instanceOf[UsualAndActualHoursView]

        val result = route(application, request).value

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form.fill(validAnswer), 1)(request, messages(application)).toString
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
          fakeRequest(POST, usualAndActualHoursRoute(1))
            .withFormUrlEncodedBody("usualHours" -> validAnswer.usualHours.toString, "actualHours" -> validAnswer.actualHours.toString)

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {

        val request =
          fakeRequest(POST, usualAndActualHoursRoute(1))
            .withFormUrlEncodedBody(("value", "invalid value"))

        val boundForm = form.bind(Map("value" -> "invalid value"))

        val view = application.injector.instanceOf[UsualAndActualHoursView]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST

        contentAsString(result) mustEqual
          view(boundForm, 1)(request, messages(application)).toString
      }
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val request = fakeRequest(GET, usualAndActualHoursRoute(1))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {
        val request =
          fakeRequest(POST, usualAndActualHoursRoute(1))
            .withFormUrlEncodedBody(("value", validAnswer.toString))

        val result = route(application, request).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }
  }
}
