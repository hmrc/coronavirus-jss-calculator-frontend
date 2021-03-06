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

import models.{Amount, UsualAndActualHours}
import org.scalacheck.Arbitrary
import pages._

trait PageGenerators {

  implicit lazy val arbitraryBusinessClosedPeriodsPage: Arbitrary[BusinessClosedPeriodsPage.type] =
    Arbitrary(BusinessClosedPeriodsPage)

  implicit lazy val arbitraryShortTermWorkingAgreementPeriodPage: Arbitrary[ShortTermWorkingAgreementPeriodPage.type] =
    Arbitrary(ShortTermWorkingAgreementPeriodPage)

  implicit lazy val arbitraryBusinessClosedPage: Arbitrary[BusinessClosedPage.type] =
    Arbitrary(BusinessClosedPage)

  implicit lazy val arbitraryTemporaryWorkingAgreementPage: Arbitrary[TemporaryWorkingAgreementPage.type] =
    Arbitrary(TemporaryWorkingAgreementPage)

  implicit lazy val arbitraryEndPayDatePage: Arbitrary[EndPayDatePage.type] =
    Arbitrary(EndPayDatePage)

  implicit lazy val arbitraryUsualAndActualHoursPage: Arbitrary[UsualAndActualHoursPage.type] =
    Arbitrary(UsualAndActualHoursPage)

  implicit lazy val arbitraryRegularPayAmountPage: Arbitrary[RegularPayAmountPage.type] =
    Arbitrary(RegularPayAmountPage)

  implicit lazy val arbitraryPayPeriodsPage: Arbitrary[PayPeriodsPage.type] =
    Arbitrary(PayPeriodsPage)

  implicit lazy val arbitraryLastPayDatePage: Arbitrary[LastPayDatePage.type] =
    Arbitrary(LastPayDatePage)

  implicit lazy val arbitraryPayFrequencyPage: Arbitrary[PayFrequencyPage.type] =
    Arbitrary(PayFrequencyPage)

  implicit lazy val arbitraryPayMethodPage: Arbitrary[PayMethodPage.type] =
    Arbitrary(PayMethodPage)

  implicit lazy val arbitraryClaimPeriodPage: Arbitrary[ClaimPeriodPage.type] =
    Arbitrary(ClaimPeriodPage)

  implicit lazy val arbitrarySelectWorkPeriodsPage: Arbitrary[SelectWorkPeriodsPage.type] =
    Arbitrary(SelectWorkPeriodsPage)

  implicit lazy val arbitraryAmount: Arbitrary[Amount] =
    Arbitrary {
      for {
        value <- Arbitrary.arbitrary[BigDecimal]
      } yield Amount(value)
    }

  implicit lazy val arbitraryUsualAndActualHours: Arbitrary[UsualAndActualHours] =
    Arbitrary {
      for {
        value <- Arbitrary.arbitrary[Double]
      } yield UsualAndActualHours(value, value)
    }

}
