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

class FourWeeklySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "4-weekly" should {

      "scenario-2" in new RegularPayGrantCalculator {

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
            LocalDate.of(2020, 11, 19)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 6),
            LocalDate.of(2020, 11, 2),
            14,
            5
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 30),
            85.4,
            23.2
          )
        )

        val maybeJobSupport: Option[JobSupport] = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FourWeekly,
          2500
        )

        maybeJobSupport.map { jobSupport =>
          jobSupport.totalGrant mustEqual 1548.03
        }
      }

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
            LocalDate.of(2020, 11, 24)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 28),
            88.45,
            17.2
          )
        )

        val maybeJobSupport: Option[JobSupport] = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.FourWeekly,
          2884.70
        )

        maybeJobSupport.map { jobSupport =>
          jobSupport.totalGrant mustEqual 1730.59
        }
      }
    }
  }
}
