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

import base.SpecBase
import org.scalatestplus.mockito.MockitoSugar
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeRequest
import play.api.test.Helpers.{GET, status, _}

class FeedbackSurveyControllerSpec extends SpecBase with MockitoSugar {

  lazy val surveyRoute = routes.FeedbackSurveyController.startSurvey().url

  private lazy val getRequest: FakeRequest[AnyContentAsEmpty.type] =
    fakeRequest(GET, surveyRoute)

  "FeedbackSurveyController" must {

    "redirect users to /feedback service" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {

        val result = route(application, getRequest).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual "http://localhost:9514/feedback/CJSSC"
      }
    }
  }
}
