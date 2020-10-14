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

package navigation

import base.SpecBase
import controllers.routes
import models._
import pages._

class NavigatorSpec extends SpecBase {

  val navigator = new Navigator

  "Navigator" when {

    "in Normal mode" must {

      "go to Index from a page that doesn't exist in the route map" in {
        case object UnknownPage extends Page
        navigator.nextPage(UnknownPage, NormalMode, emptyUserAnswers) mustBe routes.StartPageController.onPageLoad()
      }

      "go to PayFrequencyPage after ClaimPeriod" in {
        navigator.nextPage(ClaimPeriodPage, NormalMode, emptyUserAnswers) mustBe routes.PayFrequencyController.onPageLoad()
      }

      "go to PayMethodPage after PayFrequency" in {
        navigator.nextPage(PayFrequencyPage, NormalMode, emptyUserAnswers) mustBe routes.PayMethodController.onPageLoad()
      }

      "go to LastPayDatePage after PayMethod" in {
        navigator.nextPage(PayMethodPage, NormalMode, emptyUserAnswers) mustBe routes.LastPayDateController.onPageLoad()
      }
    }
  }
}
