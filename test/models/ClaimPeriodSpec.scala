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

package models

import java.time.YearMonth

import base.SpecBase
import forms.ClaimPeriodFormProvider
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.{MustMatchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.libs.json.{JsError, JsString, Json}
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text

class ClaimPeriodSpec extends SpecBase with MustMatchers with ScalaCheckPropertyChecks with OptionValues {

  "claimPeriod" must {

    "deserialise valid values" in {

      val gen = Gen.oneOf(ClaimPeriod.values)

      forAll(gen) { claimPeriod =>
        JsString(claimPeriod.toString).validate[ClaimPeriod].asOpt.value mustEqual claimPeriod
      }
    }

    "fail to deserialise invalid values" in {

      val gen = arbitrary[String] suchThat (!ClaimPeriod.values.map(_.toString).contains(_))

      forAll(gen) { invalidValue =>
        JsString(invalidValue).validate[ClaimPeriod] mustEqual JsError("error.invalid")
      }
    }

    "serialise" in {

      val gen = Gen.oneOf(ClaimPeriod.values)

      forAll(gen) { claimPeriod =>
        Json.toJson(claimPeriod) mustEqual JsString(claimPeriod.toString)
      }
    }

    "show only November 2020 in the UI when form is displayed in October 2020" in {
      val application      = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      val form             = new ClaimPeriodFormProvider()()
      val schemeEnds       = "April 2021"
      val octoberMonthYear = YearMonth.parse("October 2020", ClaimPeriod.pattern)
      val result           = ClaimPeriod.options(form, schemeEnds, octoberMonthYear)(messages(application))
      result.size mustEqual 1
      result.head.content mustEqual Text("November 2020")

    }
    "show only November 2020 in the UI when form is displayed in November 2020" in {
      val application       = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      val form              = new ClaimPeriodFormProvider()()
      val schemeEnds        = "April 2021"
      val novemberMonthYear = YearMonth.parse("November 2020", ClaimPeriod.pattern)
      val result            = ClaimPeriod.options(form, schemeEnds, novemberMonthYear)(messages(application))
      result.size mustEqual 1
      result.head.content mustEqual Text("November 2020")
    }

    "show form with expected radio buttons anytime page is displayed in 2020" in {
      val application       = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      val form              = new ClaimPeriodFormProvider()()
      val schemeEnds        = "April 2021"
      val decemberMonthYear = YearMonth.parse("December 2020", ClaimPeriod.pattern)
      val result            = ClaimPeriod.options(form, schemeEnds, decemberMonthYear)(messages(application))
      result.size mustEqual 2
      result.head.content mustEqual Text("November 2020")
      result(1).content mustEqual Text("December 2020")
    }

    "show form with expected radio buttons anytime page is displayed in 2021" in {
      val application       = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      val form              = new ClaimPeriodFormProvider()()
      val schemeEnds        = "April 2021"
      val februaryMonthYear = YearMonth.parse("February 2021", ClaimPeriod.pattern)
      val result            = ClaimPeriod.options(form, schemeEnds, februaryMonthYear)(messages(application))
      result.size mustEqual 4
      result.head.content mustEqual Text("November 2020")
      result(1).content mustEqual Text("December 2020")
      result(2).content mustEqual Text("January 2021")
      result(3).content mustEqual Text("February 2021")
    }

    "do not display any radio buttons if the page is displayed after schemeEnds" in {
      val application  = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()
      val form         = new ClaimPeriodFormProvider()()
      val schemeEnds   = "April 2021"
      val mayMonthYear = YearMonth.parse("May 2021", ClaimPeriod.pattern)
      val result       = ClaimPeriod.options(form, schemeEnds, mayMonthYear)(messages(application))
      result.size mustEqual 0
    }
  }
}
