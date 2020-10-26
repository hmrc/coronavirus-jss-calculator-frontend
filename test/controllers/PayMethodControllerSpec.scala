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
import forms.PayMethodFormProvider
import models.{PayMethod, UserAnswers}
import pages.PayMethodPage
import play.api.test.Helpers._
import views.html.PayMethodView

import scala.concurrent.ExecutionContext.Implicits.global

class PayMethodControllerSpec extends SpecBaseControllerSpecs {

  val view                            = app.injector.instanceOf[PayMethodView]
  private lazy val payMethodRouteGet  = routes.PayMethodController.onPageLoad().url
  private lazy val payMethodRoutePost = routes.PayMethodController.onSubmit().url

  private val formProvider = new PayMethodFormProvider()
  private val form         = formProvider()

  def controller(userAnswers: Option[UserAnswers]) = new PayMethodController(
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

  "PayMethod Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, payMethodRouteGet)

      val result = controller(Some(emptyUserAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set(PayMethodPage, PayMethod.values.head).success.value

      val request = fakeRequest(GET, payMethodRouteGet)

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form.fill(PayMethod.values.head))(request, messages).toString
    }

    "redirect to the next page when valid data is submitted" in {

      val request =
        fakeRequest(POST, payMethodRoutePost)
          .withFormUrlEncodedBody(("value", PayMethod.values.head.toString))

      val result = controller(Some(emptyUserAnswers)).onSubmit()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.PayPeriodsController.onPageLoad().url
    }
  }

  "return a Bad Request and errors when invalid data is submitted" in {

    val request =
      fakeRequest(POST, payMethodRoutePost)
        .withFormUrlEncodedBody(("value", "invalid value"))

    val boundForm = form.bind(Map("value" -> "invalid value"))

    val result = controller(Some(emptyUserAnswers)).onSubmit()(request)

    status(result) mustEqual BAD_REQUEST

    contentAsString(result) mustEqual
      view(boundForm)(request, messages).toString
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val request = fakeRequest(GET, payMethodRouteGet)

    val result = controller(None).onPageLoad()(request)

    status(result) mustEqual SEE_OTHER
    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }

  "redirect to Session Expired for a POST if no existing data is found" in {

    val request =
      fakeRequest(POST, payMethodRoutePost)
        .withFormUrlEncodedBody(("value", PayMethod.values.head.toString))

    val result = controller(None).onSubmit()(request)
    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
  }
}
