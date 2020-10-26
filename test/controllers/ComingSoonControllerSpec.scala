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
import play.api.mvc.AnyContentAsEmpty
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.api.test.Helpers._
import views.html.ComingSoonView

class ComingSoonControllerSpec extends SpecBaseControllerSpecs {

  val view = app.injector.instanceOf[ComingSoonView]

  private lazy val comingSoonRoute = routes.ComingSoonController.onPageLoad().url

  "ComingSoon Controller" must {

    "return OK and the correct view for a GET" in {

      val controller = new ComingSoonController(
        messagesApi,
        getSessionAction,
        stubDataRetrieval(Some(emptyUserAnswers)),
        dataRequired,
        component,
        view
      )

      val fakeRequest =
        FakeRequest("GET", comingSoonRoute).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

      val result = controller.onPageLoad()(fakeRequest)

      status(result) mustEqual OK

      contentAsString(result) mustEqual
        view()(fakeRequest, messages).toString
    }
  }
}
