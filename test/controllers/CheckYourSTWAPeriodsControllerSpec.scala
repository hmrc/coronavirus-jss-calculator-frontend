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
import models.{ShortTermWorkingAgreementPeriod, UserAnswers}
import pages.ShortTermWorkingAgreementPeriodPage
import play.api.test.Helpers._
import views.html.CheckYourSTWAPeriodsView

class CheckYourSTWAPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[CheckYourSTWAPeriodsView]

  private lazy val checkYourSTWAPeriodsRoute = routes.CheckYourSTWAPeriodsController.onPageLoad().url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  def controller(userAnswers: Option[UserAnswers]) = new CheckYourSTWAPeriodsController(
    messagesApi,
    getSessionAction,
    stubDataRetrieval(userAnswers),
    dataRequired,
    component,
    view
  )

  val startDate = LocalDate.now()
  val endDate = LocalDate.now().plusDays(10)

  val stwaPeriods = List(ShortTermWorkingAgreementPeriod(startDate, endDate))

  "CheckYourSTWAPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, checkYourSTWAPeriodsRoute)

      val result = controller(Some(emptyUserAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(Seq.empty)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val request = fakeRequest(GET, checkYourSTWAPeriodsRoute)

      val userAnswers = UserAnswers(userAnswersId).setList(ShortTermWorkingAgreementPeriodPage, stwaPeriods).success.value

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(stwaPeriods)(request, messages).toString
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val request = fakeRequest(GET, checkYourSTWAPeriodsRoute)

      val result = controller(None).onPageLoad()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }
  }
}
