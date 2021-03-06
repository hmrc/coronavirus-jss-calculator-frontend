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

import models.{BusinessClosed, ClaimPeriod, PayFrequency, PayMethod, PayPeriods, TemporaryWorkingAgreement, UsualAndActualHours}
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import pages._
import play.api.libs.json.{JsValue, Json}

trait UserAnswersEntryGenerators extends PageGenerators with ModelGenerators {
  self: Generators =>

  implicit lazy val arbitraryBusinessClosedPeriodsUserAnswersEntry
    : Arbitrary[(BusinessClosedPeriodsPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[BusinessClosedPeriodsPage.type]
        value <- arbitrary[Int].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryShortTermWorkingAgreementPeriodUserAnswersEntry
    : Arbitrary[(ShortTermWorkingAgreementPeriodPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[ShortTermWorkingAgreementPeriodPage.type]
        value <- arbitrary[Int].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryBusinessClosedUserAnswersEntry: Arbitrary[(BusinessClosedPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[BusinessClosedPage.type]
        value <- arbitrary[BusinessClosed].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryTemporaryWorkingAgreementUserAnswersEntry
    : Arbitrary[(TemporaryWorkingAgreementPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[TemporaryWorkingAgreementPage.type]
        value <- arbitrary[TemporaryWorkingAgreement].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryEndPayDateUserAnswersEntry: Arbitrary[(EndPayDatePage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[EndPayDatePage.type]
        value <- arbitrary[Int].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryUsualAndActualHoursUserAnswersEntry: Arbitrary[(UsualAndActualHoursPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[UsualAndActualHoursPage.type]
        value <- arbitrary[UsualAndActualHours].map(Json.toJson(_))
      } yield (page, value)
    }

  implicit lazy val arbitraryRegularPayAmountUserAnswersEntry: Arbitrary[(RegularPayAmountPage.type, JsValue)] =
    Arbitrary {
      for {
        page  <- arbitrary[RegularPayAmountPage.type]
        value <- arbitrary(arbBigDecimal).map(Json.toJson(_))
      } yield (page, value)
    }

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
