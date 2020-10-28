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

import java.time.{Instant, LocalDate, ZoneOffset}

import models._
import org.scalacheck.{Arbitrary, Gen}

trait ModelGenerators {
  self: Generators =>

  implicit lazy val arbitraryBusinessClosed: Arbitrary[BusinessClosed] =
    Arbitrary {
      Gen.oneOf(BusinessClosed.values)
    }

  implicit lazy val genTemporaryWorkingAgreementPeriod: Gen[TemporaryWorkingAgreementPeriod] =
    for {
      period <- periodGen
    } yield TemporaryWorkingAgreementPeriod(period.startDate, period.endDate)

  implicit lazy val genBusinessClosedPeriods: Gen[BusinessClosedPeriod] =
    for {
      period <- periodGen
    } yield BusinessClosedPeriod(period.startDate, period.endDate)

  implicit lazy val arbitraryShortTermWorkingAgreementPeriod: Arbitrary[TemporaryWorkingAgreementPeriod] =
    Arbitrary {
      genTemporaryWorkingAgreementPeriod
    }

  implicit lazy val arbitraryBusinessClosedPeriods: Arbitrary[BusinessClosedPeriod] =
    Arbitrary {
      genBusinessClosedPeriods
    }

  implicit lazy val arbitraryTemporaryWorkingAgreement: Arbitrary[TemporaryWorkingAgreement] =
    Arbitrary {
      Gen.oneOf(TemporaryWorkingAgreement.values)
    }

  implicit lazy val arbitraryPayPeriods: Arbitrary[PayPeriods] =
    Arbitrary {
      Gen.oneOf(PayPeriods.values)
    }

  implicit lazy val arbitraryPayFrequency: Arbitrary[PayFrequency] =
    Arbitrary {
      Gen.oneOf(PayFrequency.values)
    }

  implicit lazy val arbitraryPayMethod: Arbitrary[PayMethod] =
    Arbitrary {
      Gen.oneOf(PayMethod.values)
    }

  implicit lazy val arbitraryClaimPeriod: Arbitrary[ClaimPeriod] =
    Arbitrary {
      Gen.oneOf(ClaimPeriod.values)
    }

  implicit lazy val arbitraryDates: Arbitrary[LocalDate] = Arbitrary(datesGen)

  val datesGen = for {
    date <- periodDatesBetween(LocalDate.of(2020, 11, 1), LocalDate.of(2021, 6, 30))
  } yield date

  def periodDatesBetween(min: LocalDate, max: LocalDate): Gen[LocalDate] = {

    def toMillis(date: LocalDate): Long =
      date.atStartOfDay.atZone(ZoneOffset.UTC).toInstant.toEpochMilli

    Gen.choose(toMillis(min), toMillis(max)).map { millis =>
      Instant.ofEpochMilli(millis).atOffset(ZoneOffset.UTC).toLocalDate
    }
  }

  val periodGen: Gen[Period] =
    for {
      date <- datesGen
    } yield Period(date, date.plusDays(10))

  implicit lazy val arbitraryPeriod: Arbitrary[Period] =
    Arbitrary {
      periodGen
    }
}
