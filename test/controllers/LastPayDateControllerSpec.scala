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
import forms.LastPayDateFormProvider
import models.UserAnswers
import pages.{ClaimPeriodPage, LastPayDatePage}
import play.api.libs.json.{JsString, Json}
import play.api.mvc.{AnyContentAsEmpty, AnyContentAsFormUrlEncoded}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.LastPayDateView

import scala.concurrent.ExecutionContext.Implicits.global

class LastPayDateControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[LastPayDateView]

  private val formProvider = new LastPayDateFormProvider()

  private def form = formProvider()

  private val validAnswer = LocalDate.now(ZoneOffset.UTC)

  private lazy val lastPayDateRouteGet = routes.LastPayDateController.onPageLoad().url
  private lazy val lastPayDateRoutePost = routes.LastPayDateController.onSubmit().url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val getRequest: FakeRequest[AnyContentAsEmpty.type] =
    fakeRequest(GET, lastPayDateRouteGet)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, lastPayDateRoutePost)
      .withFormUrlEncodedBody(
        "value.day"   -> validAnswer.getDayOfMonth.toString,
        "value.month" -> validAnswer.getMonthValue.toString,
        "value.year"  -> validAnswer.getYear.toString
      )

  val claimPeriod = LocalDate.of(2020, 11, 1)

  val userAnswers = UserAnswers(userAnswersId, Json.obj(ClaimPeriodPage.toString -> JsString("November 2020")))

  def controller(userAnswers: Option[UserAnswers]) = new LastPayDateController(
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

  "LastPayDate Controller" must {

    "return OK and the correct view for a GET" in {

      val fakeRequest = getRequest

      val result = controller(Some(userAnswers)).onPageLoad()(fakeRequest)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, claimPeriod)(getRequest, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers =
        UserAnswers(userAnswersId, Json.obj(LastPayDatePage.toString -> validAnswer, ClaimPeriodPage.toString -> JsString("November 2020")))

      val fakeRequest = getRequest

      val result = controller(Some(userAnswers)).onPageLoad()(fakeRequest)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(validAnswer), claimPeriod)(getRequest, messages).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val fakeRequest = postRequest

      val result = controller(Some(userAnswers)).onSubmit()(fakeRequest)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.LastPayDateController.onPageLoad().url
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val request =
        fakeRequest(POST, lastPayDateRoutePost)
          .withFormUrlEncodedBody(("value", "invalid value"))

      val boundForm = form.bind(Map("value" -> "invalid value"))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, claimPeriod)(request, messages).toString
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val fakeRequest = getRequest

      val result = controller(None).onPageLoad()(fakeRequest)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val fakeRequest = postRequest

      val result = controller(None).onSubmit()(fakeRequest)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }

  }
}
