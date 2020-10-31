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
import forms.EndPayDateFormProvider
import models.{ClaimPeriod, UserAnswers}
import pages.{ClaimPeriodPage, EndPayDatePage, LastPayDatePage}
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.EndPayDateView

import scala.concurrent.ExecutionContext.Implicits.global

class EndPayDateControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[EndPayDateView]

  private val formProvider = new EndPayDateFormProvider()
  private def form         = formProvider

  private lazy val endPayDateRouteGet  = routes.EndPayDateController.onPageLoad().url
  private lazy val endPayDateRoutePost = routes.EndPayDateController.onSubmit().url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val getRequest: FakeRequest[AnyContentAsEmpty.type] =
    fakeRequest(GET, endPayDateRouteGet)

  val lastPayDate    = LocalDate.of(2020, 10, 10)
  val claimStartDate = LocalDate.of(2020, 11, 1)
  val claimEndDate   = LocalDate.of(2020, 11, 30)

  private val validAnswer = lastPayDate.plusDays(30)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, endPayDateRoutePost)
      .withFormUrlEncodedBody(
        "value.day"   -> validAnswer.getDayOfMonth.toString,
        "value.month" -> validAnswer.getMonthValue.toString,
        "value.year"  -> validAnswer.getYear.toString
      )

  val userAnswers = emptyUserAnswers
    .set(LastPayDatePage, lastPayDate)
    .success
    .value
    .set(ClaimPeriodPage, ClaimPeriod.Nov2020)
    .success
    .value

  def controller(userAnswers: Option[UserAnswers]) = new EndPayDateController(
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

  "EndPayDate Controller" must {

    "return OK and the correct view for a GET" in {

      val fakeRequest = getRequest

      val result = controller(Some(userAnswers)).onPageLoad()(fakeRequest)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form(lastPayDate, claimStartDate, claimEndDate), lastPayDate)(getRequest, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(EndPayDatePage, validAnswer).success.value

      val fakeRequest = getRequest

      val result = controller(Some(userAnswersUpdated)).onPageLoad()(fakeRequest)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form(lastPayDate, claimStartDate, claimEndDate).fill(validAnswer), lastPayDate)(
          getRequest,
          messages
        ).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val fakeRequest = postRequest

      val result = controller(Some(userAnswers)).onSubmit()(fakeRequest)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.PayMethodController.onPageLoad().url
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val request =
        fakeRequest(POST, endPayDateRoutePost)
          .withFormUrlEncodedBody(("value", "invalid value"))

      val boundForm = form(lastPayDate, claimStartDate, claimEndDate).bind(Map("value" -> "invalid value"))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, lastPayDate)(request, messages).toString
    }
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val fakeRequest = getRequest

    val result = controller(None).onPageLoad()(fakeRequest)

    status(result) mustEqual SEE_OTHER
    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }

  "redirect to Session Expired for a POST if no existing data is found" in {

    val fakeRequest = postRequest

    val result = controller(None).onPageLoad()(fakeRequest)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }
}
