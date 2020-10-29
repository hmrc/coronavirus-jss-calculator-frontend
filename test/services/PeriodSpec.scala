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
import models.{BusinessClosedPeriod, JobSupport, PayFrequency, PayPeriod, SupportClaimPeriod, TemporaryWorkingAgreementPeriod}

class PeriodSpec extends SpecBase {

  "Regular Pay Calculator" when {

    "computing the JSS-O on weekly freq" should {

      "scenario-9" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 6),
            6,
            6
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 13),
            12,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 14),
            LocalDate.of(2020, 11, 20),
            24,
            12
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 21),
            LocalDate.of(2020, 11, 27),
            12,
            12
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          1000
        )

        println(s"\n Job support is : \n${jobSupport.supportBreakdown.mkString("\n\n")}\n")

        jobSupport.totalEmployeeSalary mustEqual 273.98
        jobSupport.totalEmployersGrant mustEqual 253.44
        jobSupport.totalClosed mustEqual 1232.89
        jobSupport.totalGrant mustEqual 1486.33
      }
    }
  }
}
