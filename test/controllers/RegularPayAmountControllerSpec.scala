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
import forms.RegularPayAmountFormProvider
import models.{Amount, UserAnswers}
import pages.{LastPayDatePage, RegularPayAmountPage}
import play.api.test.Helpers._
import views.html.RegularPayAmountView

import scala.concurrent.ExecutionContext.Implicits.global

class RegularPayAmountControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[RegularPayAmountView]

  private val formProvider = new RegularPayAmountFormProvider()
  private val form = formProvider()

  private lazy val regularPayAmountRouteGet = routes.RegularPayAmountController.onPageLoad().url
  private lazy val regularPayAmountRoutePost = routes.RegularPayAmountController.onSubmit().url

  val lastPayDate = LocalDate.of(2020, 10, 30)

  val userAnswers = emptyUserAnswers.set(LastPayDatePage, lastPayDate).success.value

  def controller(userAnswers: Option[UserAnswers]) = new RegularPayAmountController(
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

  "RegularPayAmount Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, regularPayAmountRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, lastPayDate)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswersUpdated = userAnswers.set(RegularPayAmountPage, Amount(10.99)).success.value

      val request = fakeRequest(GET, regularPayAmountRouteGet)

      val result = controller(Some(userAnswersUpdated)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(Amount(10.99)), lastPayDate)(request, messages).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val request =
        fakeRequest(POST, regularPayAmountRoutePost)
          .withFormUrlEncodedBody(("value", "10.20"))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual SEE_OTHER
      redirectLocation(result).value mustEqual routes.TemporaryWorkingAgreementController.onPageLoad().url
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val request =
        fakeRequest(POST, regularPayAmountRoutePost)
          .withFormUrlEncodedBody(("value", ""))

      val boundForm = form.bind(Map("value" -> ""))

      val result = controller(Some(userAnswers)).onSubmit()(request)

      status(result) mustEqual BAD_REQUEST

      contentAsString(result) mustEqual
        view(boundForm, lastPayDate)(request, messages).toString
    }
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val request = fakeRequest(GET, regularPayAmountRouteGet)

    val result = controller(None).onPageLoad()(request)

    status(result) mustEqual SEE_OTHER
    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }

  "redirect to Session Expired for a POST if no existing data is found" in {

    val request =
      fakeRequest(POST, regularPayAmountRoutePost)
        .withFormUrlEncodedBody(("value", "answer"))

    val result = controller(None).onSubmit()(request)
    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }
}
