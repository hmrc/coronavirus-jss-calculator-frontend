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
import forms.UsualAndActualHoursFormProvider
import models.{ClaimPeriod, Period, UserAnswers, UsualAndActualHours}
import pages.{ClaimPeriodPage, SelectWorkPeriodsPage, UsualAndActualHoursPage}
import play.api.mvc.AnyContentAsEmpty
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.UsualAndActualHoursView

import scala.concurrent.ExecutionContext.Implicits.global

class UsualAndActualHoursControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[UsualAndActualHoursView]

  private val formProvider = new UsualAndActualHoursFormProvider()
  private val form = formProvider()

  private val validAnswer = UsualAndActualHours(10.00, 20.00)

  private def usualAndActualHoursRouteGet(idx: Int) = routes.UsualAndActualHoursController.onPageLoad(idx).url
  private def usualAndActualHoursRoutePost(idx: Int) = routes.UsualAndActualHoursController.onSubmit(idx).url

  def getRequest(method: String, idx: Int) =
    FakeRequest(method, usualAndActualHoursRouteGet(idx)).withCSRFToken
      .asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  def controller(userAnswers: Option[UserAnswers]) = new UsualAndActualHoursController(
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

  val startDate = LocalDate.of(2020, 10, 31)
  val endDate = LocalDate.of(2020, 11, 29)

  val periods = List(Period(startDate, endDate))

  val startDateToShow = ClaimPeriod.Nov2020.supportClaimPeriod.startDate

  val userAnswers = emptyUserAnswers
    .set(SelectWorkPeriodsPage, periods)
    .success
    .value
    .set(ClaimPeriodPage, ClaimPeriod.Nov2020)
    .success
    .value

  "UsualAndActualHours Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, usualAndActualHoursRouteGet(1))

      val result = controller(Some(userAnswers)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, 1, startDateToShow, endDate)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(UsualAndActualHoursPage, validAnswer, Some(1)).success.value

      val request = fakeRequest(GET, usualAndActualHoursRouteGet(1))

      val result = controller(Some(userAnswersUpdated)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(validAnswer), 1, startDateToShow, endDate)(request, messages).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val request =
        fakeRequest(POST, usualAndActualHoursRoutePost(1))
          .withFormUrlEncodedBody("usualHours" -> validAnswer.usualHours.toString, "actualHours" -> validAnswer.actualHours.toString)

      val result = controller(Some(userAnswers)).onSubmit(1)(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.ConfirmationController.onPageLoad().url
    }
  }

  "return a Bad Request and errors when invalid data is submitted" in {

    val request =
      fakeRequest(POST, usualAndActualHoursRoutePost(1))
        .withFormUrlEncodedBody(("value", "invalid value"))

    val boundForm = form.bind(Map("value" -> "invalid value"))

    val result = controller(Some(userAnswers)).onSubmit(1)(request)

    status(result) mustEqual BAD_REQUEST

    contentAsString(result) mustEqual
      view(boundForm, 1, startDateToShow, endDate)(request, messages).toString
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val request = fakeRequest(GET, usualAndActualHoursRouteGet(1))

    val result = controller(None).onPageLoad(1)(request)

    status(result) mustEqual SEE_OTHER
    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }

  "redirect to Session Expired for a POST if no existing data is found" in {
    val request =
      fakeRequest(POST, usualAndActualHoursRoutePost(1))
        .withFormUrlEncodedBody(("value", validAnswer.toString))

    val result = controller(None).onSubmit(1)(request)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }
}
