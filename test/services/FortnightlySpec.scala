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

class FortnightlySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "fortnight" should {

      "scenario-1" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 15)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 19),
            LocalDate.of(2020, 11, 1),
            10,
            5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 15),
            90.4,
            30
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 29),
            89.5,
            30
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FortNightly,
          650
        )

        jobSupport.totalGrant mustEqual 714.15

      }

      "scenario-9" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 11),
            10,
            4.5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 25),
            38.9,
            6
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FortNightly,
          1410.10
        )

        jobSupport.totalGrant mustEqual 1543.42

      }

      "scenario-10" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 11),
            10,
            4.5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 25),
            38.9,
            6
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FortNightly,
          1439.10
        )

        jobSupport.totalGrant mustEqual 1574.34
      }
    }
  }
}
