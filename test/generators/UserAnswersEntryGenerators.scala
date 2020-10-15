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

package generators

import java.time.LocalDate

import models.{ClaimPeriod, PayFrequency, PayMethod, PayPeriods}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import pages._
import play.api.libs.json.{JsValue, Json}

trait UserAnswersEntryGenerators extends PageGenerators with ModelGenerators {
  self: Generators =>

  implicit lazy val arbitraryPayPeriodsUserAnswersEntry: Arbitrary[(PayPeriodsPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[PayPeriodsPage.type]
        value <- arbitrary[PayPeriods].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryLastPayDateUserAnswersEntry: Arbitrary[(LastPayDatePage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[LastPayDatePage.type]
        value <- arbitrary[Int].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryPayFrequencyUserAnswersEntry: Arbitrary[(PayFrequencyPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[PayFrequencyPage.type]
        value <- arbitrary[PayFrequency].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryPayMethodUserAnswersEntry: Arbitrary[(PayMethodPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[PayMethodPage.type]
        value <- arbitrary[PayMethod].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryClaimPeriodUserAnswersEntry: Arbitrary[(ClaimPeriodPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[ClaimPeriodPage.type]
        value <- arbitrary[ClaimPeriod].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitrarySelectWorkPeriodsUserAnswersEntry: Arbitrary[(SelectWorkPeriodsPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[SelectWorkPeriodsPage.type]
        value <- arbitrary[List[LocalDate]].map(Json.toJson(_))
      } yield (page, value)
    }

}
