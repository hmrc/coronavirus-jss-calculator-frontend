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

import base.SpecBaseControllerSpecs
import forms.BusinessClosedPeriodsFormProvider
import models.{BusinessClosedPeriods, UserAnswers}
import pages.BusinessClosedPeriodsPage
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.BusinessClosedPeriodsView

import scala.concurrent.ExecutionContext.Implicits.global

class BusinessClosedPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[BusinessClosedPeriodsView]

  private val formProvider = new BusinessClosedPeriodsFormProvider()
  private def form         = formProvider()

  private val validAnswer = LocalDate.now(ZoneOffset.UTC)

  private lazy val businessClosedPeriodsRouteGet  = routes.BusinessClosedPeriodsController.onPageLoad(1).url
  private lazy val businessClosedPeriodsRoutePost = routes.BusinessClosedPeriodsController.onSubmit(1).url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, businessClosedPeriodsRoutePost)
      .withFormUrlEncodedBody(
        "startDate.day"   -> validAnswer.getDayOfMonth.toString,
        "startDate.month" -> validAnswer.getMonthValue.toString,
        "startDate.year"  -> validAnswer.getYear.toString,
        "endDate.day"     -> validAnswer.getDayOfMonth.toString,
        "endDate.month"   -> validAnswer.getMonthValue.toString,
        "endDate.year"    -> validAnswer.getYear.toString
      )

  def controller(userAnswers: Option[UserAnswers]) = new BusinessClosedPeriodsController(
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

  val bcPeriods = BusinessClosedPeriods(validAnswer, validAnswer.plusDays(1))

  "BusinessClosedPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, businessClosedPeriodsRouteGet)

      val result = controller(Some(emptyUserAnswers)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, 1)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set(BusinessClosedPeriodsPage, bcPeriods, Some(1)).success.value

      val request = fakeRequest(GET, businessClosedPeriodsRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(bcPeriods), 1)(request, messages).toString
    }
  }

  "redirect to add more dates when valid data is submitted and radio answer is yes" in {

    val result = controller(Some(emptyUserAnswers)).onSubmit(1)(postRequest)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.CheckYourBusinessClosedPeriodsController.onPageLoad().url
  }

  "redirect to usual and actual hours page when valid data is submitted and radio answer is no" in {

    val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
      fakeRequest(POST, businessClosedPeriodsRoutePost)
        .withFormUrlEncodedBody(
          "startDate.day"   -> validAnswer.getDayOfMonth.toString,
          "startDate.month" -> validAnswer.getMonthValue.toString,
          "startDate.year"  -> validAnswer.getYear.toString,
          "endDate.day"     -> validAnswer.getDayOfMonth.toString,
          "endDate.month"   -> validAnswer.getMonthValue.toString,
          "endDate.year"    -> validAnswer.getYear.toString
        )

    val result = controller(Some(emptyUserAnswers)).onSubmit(1)(postRequest)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.CheckYourBusinessClosedPeriodsController.onPageLoad().url
  }

  "return a Bad Request and errors when invalid data is submitted" in {

    val request =
      fakeRequest(POST, businessClosedPeriodsRoutePost)
        .withFormUrlEncodedBody(("value", "invalid value"))

    val result = controller(Some(emptyUserAnswers)).onSubmit(1)(request)

    status(result) mustEqual BAD_REQUEST
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val request = fakeRequest(GET, businessClosedPeriodsRouteGet)

    val result = controller(None).onPageLoad(1)(request)

    status(result) mustEqual SEE_OTHER
    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }

  "redirect to Session Expired for a POST if no existing data is found" in {
    val result = controller(None).onSubmit(1)(postRequest)
    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }
}
