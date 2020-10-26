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

class FourWeeklySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "4 weekly" should {

      "sc2" in new RegularPayGrantCalculator {

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
            LocalDate.of(2020, 11, 19)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 6),
            LocalDate.of(2020, 11, 2),
            14,
            5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 30),
            85.4,
            23.2
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FourWeekly,
          2500
        )

        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 1547.9785714285713
      }

    }

  }
}
