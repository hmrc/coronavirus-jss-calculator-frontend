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

class BugSpec extends SpecBase {

  "Regular Pay Calculator" when {

    "computing the JSS-O on weekly freq" should {

      "bug-1" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 7)
          ),
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 14)
          )
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 15)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 28),
            LocalDate.of(2020, 11, 3),
            0,
            0
          )
//          PayPeriod(
//            LocalDate.of(2020, 11, 4),
//            LocalDate.of(2020, 11, 10),
//            0,
//            0
//          ),
//          PayPeriod(
//            LocalDate.of(2020, 11, 11),
//            LocalDate.of(2020, 11, 17),
//            0,
//            0
//          ),
//          PayPeriod(
//            LocalDate.of(2020, 11, 18),
//            LocalDate.of(2020, 11, 24),
//            0,
//            0
//          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          500
        )

        jobSupport.totalEmployeeSalary mustEqual 349.23
        jobSupport.totalEmployersGrant mustEqual 323.03
        jobSupport.totalClosed mustEqual 857.16
        jobSupport.totalGrant mustEqual 1180.19
      }

    }
  }
}
