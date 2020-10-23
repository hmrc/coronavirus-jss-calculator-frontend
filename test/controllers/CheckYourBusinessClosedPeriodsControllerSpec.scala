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
import models.{BusinessClosedPeriods, UserAnswers}
import pages.BusinessClosedPeriodsPage
import play.api.test.Helpers._
import views.html.CheckYourBusinessClosedPeriodsView

class CheckYourBusinessClosedPeriodsControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[CheckYourBusinessClosedPeriodsView]

  private lazy val checkYourBusinessClosedPeriodsRoute = routes.CheckYourBusinessClosedPeriodsController.onPageLoad().url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  def controller(userAnswers: Option[UserAnswers]) = new CheckYourBusinessClosedPeriodsController(
    messagesApi,
    getSessionAction,
    stubDataRetrieval(userAnswers),
    dataRequired,
    component,
    view
  )

  val startDate = LocalDate.now()
  val endDate = LocalDate.now().plusDays(10)

  val bcPeriods = List(BusinessClosedPeriods(startDate, endDate))

  "CheckYourSTWAPeriods Controller" must {

    "return OK and the correct view for a GET" in {

      val request = fakeRequest(GET, checkYourBusinessClosedPeriodsRoute)

      val result = controller(Some(emptyUserAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(Seq.empty)(request, messages).toString
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val request = fakeRequest(GET, checkYourBusinessClosedPeriodsRoute)

      val userAnswers = UserAnswers(userAnswersId).setList(BusinessClosedPeriodsPage, bcPeriods).success.value

      val result = controller(Some(userAnswers)).onPageLoad()(request)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view(bcPeriods)(request, messages).toString
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val request = fakeRequest(GET, checkYourBusinessClosedPeriodsRoute)

      val result = controller(None).onPageLoad()(request)

      status(result) mustEqual SEE_OTHER

      redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
    }
  }
}