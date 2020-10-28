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
import models.{BusinessClosedWithDates, JobSupport, PayFrequency, PeriodWithHours, SupportClaimPeriod, TemporaryWorkingAgreementWithDates}

class MonthlySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "monthly " should {

      "scenario-5" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 12)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 26)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 25),
            320.12,
            145.55
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Monthly,
          2345.67
        )

        jobSupport.totalGrant mustEqual 657.58
      }

      "sc-6" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 15)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 28)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 28),
            310.15,
            134.44
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Monthly,
          3330.00
        )

        jobSupport.totalGrant mustEqual 1428.85
      }
    }
  }
}
