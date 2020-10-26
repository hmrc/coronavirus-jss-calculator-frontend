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
import forms.ShortTermWorkingAgreementPeriodFormProvider
import models.{ShortTermWorkingAgreementPeriod, UserAnswers}
import pages.ShortTermWorkingAgreementPeriodPage
import play.api.mvc.AnyContentAsFormUrlEncoded
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.ShortTermWorkingAgreementPeriodView

import scala.concurrent.ExecutionContext.Implicits.global

class ShortTermWorkingAgreementPeriodControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[ShortTermWorkingAgreementPeriodView]

  private val formProvider = new ShortTermWorkingAgreementPeriodFormProvider()
  private def form         = formProvider()

  private val validAnswer = LocalDate.now(ZoneOffset.UTC)

  private lazy val shortTermWorkingAgreementPeriodRouteGet  =
    routes.ShortTermWorkingAgreementPeriodController.onPageLoad(1).url
  private lazy val shortTermWorkingAgreementPeriodRoutePost =
    routes.ShortTermWorkingAgreementPeriodController.onSubmit(1).url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, shortTermWorkingAgreementPeriodRoutePost)
      .withFormUrlEncodedBody(
        "startDate.day"   -> validAnswer.getDayOfMonth.toString,
        "startDate.month" -> validAnswer.getMonthValue.toString,
        "startDate.year"  -> validAnswer.getYear.toString,
        "endDate.day"     -> validAnswer.getDayOfMonth.toString,
        "endDate.month"   -> validAnswer.getMonthValue.toString,
        "endDate.year"    -> validAnswer.getYear.toString
      )

  def controller(userAnswers: Option[UserAnswers]) = new ShortTermWorkingAgreementPeriodController(
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

  val stwa = ShortTermWorkingAgreementPeriod(validAnswer, validAnswer.plusDays(10))

  "ShortTermWorkingAgreementPeriod Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, shortTermWorkingAgreementPeriodRouteGet)

      val result = controller(Some(emptyUserAnswers)).onPageLoad(1)(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(form, 1)(request, messages).toString
    }
  }

  "populate the view correctly on a GET when the question has previously been answered" in {

    val userAnswers = UserAnswers(userAnswersId).set(ShortTermWorkingAgreementPeriodPage, stwa, Some(1)).success.value

    val request = fakeRequest(GET, shortTermWorkingAgreementPeriodRouteGet)

    val result = controller(Some(userAnswers)).onPageLoad(1)(request)

    status(result) mustEqual OK

    contentAsString(result) mustEqual
      view(form.fill(stwa), 1)(request, messages).toString
  }

  "redirect to check-your-answers when valid data is submitted" in {

    val result = controller(Some(emptyUserAnswers)).onSubmit(1)(postRequest)

    status(result) mustEqual SEE_OTHER

    redirectLocation(result).value mustEqual routes.CheckYourSTWAPeriodsController.onPageLoad().url
  }

  "return a Bad Request and errors when invalid data is submitted" in {
    val request =
      fakeRequest(POST, shortTermWorkingAgreementPeriodRoutePost)
        .withFormUrlEncodedBody(("value", "invalid value"))

    val result = controller(Some(emptyUserAnswers)).onSubmit(1)(request)

    status(result) mustEqual BAD_REQUEST
  }

  "redirect to Session Expired for a GET if no existing data is found" in {

    val request = fakeRequest(GET, shortTermWorkingAgreementPeriodRouteGet)

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
