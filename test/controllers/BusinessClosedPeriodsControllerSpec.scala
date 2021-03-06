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
import forms.BusinessClosedPeriodsFormProvider
import models.{BusinessClosedPeriod, ClaimPeriod, UserAnswers}
import pages.{BusinessClosedPeriodsPage, ClaimPeriodPage}
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.BusinessClosedPeriodsView

import scala.concurrent.ExecutionContext.Implicits.global

class BusinessClosedPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[BusinessClosedPeriodsView]

  private val formProvider = app.injector.instanceOf[BusinessClosedPeriodsFormProvider]
  private def form         = formProvider

  private val startDate = LocalDate.of(2020, 11, 1)
  private val endDate   = startDate.plusDays(10)

  private lazy val businessClosedPeriodsRouteGet  = routes.BusinessClosedPeriodsController.onPageLoad(1).url
  private lazy val businessClosedPeriodsRoutePost = routes.BusinessClosedPeriodsController.onSubmit(1).url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, businessClosedPeriodsRoutePost)
      .withFormUrlEncodedBody(
        "startDate.day"   -> startDate.getDayOfMonth.toString,
        "startDate.month" -> startDate.getMonthValue.toString,
        "startDate.year"  -> startDate.getYear.toString,
        "endDate.day"     -> endDate.getDayOfMonth.toString,
        "endDate.month"   -> endDate.getMonthValue.toString,
        "endDate.year"    -> endDate.getYear.toString,
        "addAnother"      -> "true"
      )

  val claimPeriod = ClaimPeriod.Nov2020

  val userAnswers = emptyUserAnswers.set(ClaimPeriodPage, claimPeriod).success.value

  def controller(userAnswers: Option[UserAnswers]) = new BusinessClosedPeriodsController(
    messagesApi,
    mockSessionRepository,
    navigator,
    getSessionAction,
    stubDataRetrieval(userAnswers),
    dataRequired,
    formProvider,
    component,
    view,
    frontendAppConfig
  )

  val bcPeriods = BusinessClosedPeriod(startDate, endDate)

  "BusinessClosedPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, businessClosedPeriodsRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form(List.empty), 1, 5)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(BusinessClosedPeriodsPage, bcPeriods, Some(1)).success.value

      val request = fakeRequest(GET, businessClosedPeriodsRouteGet)

      val result = controller(Some(userAnswersUpdated)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form(List.empty).fill(bcPeriods), 1, 5)(request, messages).toString
    }
  }

  "redirect to the next page when valid data is submitted" in {

    val result = controller(Some(userAnswers)).onSubmit(1)(postRequest)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.BusinessClosedPeriodsController.onPageLoad(2).url
  }

  "return a Bad Request and errors when invalid data is submitted" in {

    val request =
      fakeRequest(POST, businessClosedPeriodsRoutePost)
        .withFormUrlEncodedBody(("value", "invalid value"))

    val result = controller(Some(userAnswers)).onSubmit(1)(request)

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
