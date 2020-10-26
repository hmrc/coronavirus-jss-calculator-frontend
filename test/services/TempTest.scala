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

package services

import java.time.LocalDate

import base.SpecBase
import models.{BusinessClosedWithDates, TemporaryWorkingAgreementWithDates}

class TempTest extends SpecBase {

  "Temp Test" when {

    "computing the overlap" should {

      "not overlap as before start of twa period" in new RegularPayGrantCalculator {

        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 10, 26), LocalDate.of(2020, 10, 29))

        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe false

      }

      "not overlap as after end of twa period" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 12, 1), LocalDate.of(2020, 12, 3))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe false
      }

      "overlap as end date matches start date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 10, 26), LocalDate.of(2020, 11, 1))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as end date after start date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 10, 26), LocalDate.of(2020, 11, 3))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date on start date of twa and end date after start date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 3))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date and end date on  start date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 1))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date and end date in between start date and end date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 4), LocalDate.of(2020, 11, 7))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date before end date of twa and end date is on end date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 25), LocalDate.of(2020, 11, 30))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date is on end date and end date is after end date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 30), LocalDate.of(2020, 12, 23))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

      "overlap as start date is before end date of twa and end date is after end date of twa" in new RegularPayGrantCalculator {
        val temporaryWorkingAgreementWithDates =
          TemporaryWorkingAgreementWithDates(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        val businessClosedWithDates            = BusinessClosedWithDates(LocalDate.of(2020, 11, 15), LocalDate.of(2020, 12, 23))
        hasOverlappingTwaAndBusinessClosedPeriods(
          List(temporaryWorkingAgreementWithDates),
          List(businessClosedWithDates)
        ) mustBe true
      }

    }
  }
}
