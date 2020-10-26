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

import base.SpecBaseControllerSpecs
import forms.ClaimPeriodFormProvider
import models.{ClaimPeriod, UserAnswers}
import pages.ClaimPeriodPage
import play.api.mvc.AnyContentAsEmpty
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.ClaimPeriodView

import scala.concurrent.ExecutionContext.Implicits.global

class ClaimPeriodControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[ClaimPeriodView]

  private lazy val claimPeriodRoute = routes.ClaimPeriodController.onPageLoad().url

  private val formProvider = new ClaimPeriodFormProvider()
  private val form         = formProvider()

  def controller(stubbedAnswer: UserAnswers = emptyUserAnswers) =
    new ClaimPeriodController(
      messagesApi,
      mockSessionRepository,
      navigator,
      getSessionAction,
      stubDataRetrieval(Some(stubbedAnswer)),
      formProvider,
      component,
      view
    )

  def getRequest(method: String) =
    FakeRequest(method, claimPeriodRoute).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  "claimPeriod Controller" must {

    "return OK and the correct view for a GET" in {

      val controller1 = controller(emptyUserAnswers)
      val request     = getRequest(GET)

      val result = controller1.onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form)(request, messages, frontendAppConfig).toString
    }
  }

  "populate the view correctly on a GET when the question has previously been answered" in {
    val userAnswers = emptyUserAnswers.set(ClaimPeriodPage, ClaimPeriod.values.head).success.value
    val controller1 = controller(userAnswers)
    val request     = getRequest(GET)

    val result = controller1.onPageLoad()(request)

    status(result) mustEqual OK

    contentAsString(result) mustEqual
      view(form.fill(ClaimPeriod.values.head))(request, messages, frontendAppConfig).toString
  }

  "redirect to the next page when valid data is submitted" in {

    val userAnswers = emptyUserAnswers.set(ClaimPeriodPage, ClaimPeriod.values.head).success.value
    val controller1 = controller(userAnswers)
    val request     = getRequest(POST).withFormUrlEncodedBody(("value", ClaimPeriod.values.head.toString))
    val result      = controller1.onSubmit()(request)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.PayFrequencyController.onPageLoad().url
  }

  "return a Bad Request and errors when invalid data is submitted" in {
    val userAnswers = emptyUserAnswers.set(ClaimPeriodPage, ClaimPeriod.values.head).success.value
    val controller1 = controller(userAnswers)
    val request     = getRequest(POST).withFormUrlEncodedBody(("value", "invalid value"))
    val result      = controller1.onSubmit()(request)
    val boundForm   = form.bind(Map("value" -> "invalid value"))
    status(result) mustEqual BAD_REQUEST

    contentAsString(result) mustEqual
      view(boundForm)(request, messages, frontendAppConfig).toString
  }
}
