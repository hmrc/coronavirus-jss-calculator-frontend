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

class WeeklySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "computing the JSS-O on weekly freq" should {

      "scenario-1" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val temporaryWorkingAgreementPeriods = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val businessClosedPeriods = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 16)
          )
        )

        val payPeriods: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            15,
            7.5
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            15,
            7.5
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            15,
            7.5
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            37.5,
            15
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 30),
            37.5,
            15
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          payPeriods,
          temporaryWorkingAgreementPeriods,
          businessClosedPeriods,
          PayFrequency.Weekly,
          300
        )

        println(s"Job support: ${jobSupport.supportBreakdown.mkString("\n\n")}")

        jobSupport.totalEmployeeSalary mustEqual 268.59
        jobSupport.totalEmployersGrant mustEqual 248.45
        jobSupport.totalClosed mustEqual 400.02
        jobSupport.totalGrant mustEqual 648.47
      }

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
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 19),
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 1),
            5.7,
            2
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 8),
            40,
            25
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 9),
            LocalDate.of(2020, 11, 15),
            40,
            22
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22),
            40,
            10
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 23),
            LocalDate.of(2020, 11, 29),
            40,
            15
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          350
        )

        jobSupport.totalEmployeeSalary mustEqual 252.48
        jobSupport.totalEmployersGrant mustEqual 233.55
        jobSupport.totalClosed mustEqual 533.37
        jobSupport.totalGrant mustEqual 766.92
      }

      "scenario-3" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 7),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 14),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 15),
            LocalDate.of(2020, 11, 21),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 22),
            LocalDate.of(2020, 11, 28),
            0,
            0
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          525.56
        )

        jobSupport.totalEmployeeSalary mustEqual 0.0
        jobSupport.totalEmployersGrant mustEqual 0.0
        jobSupport.totalClosed mustEqual 1401.56
        jobSupport.totalGrant mustEqual 1401.56
      }

      "scenario-4" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 21)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 4),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 11),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 18),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 19),
            LocalDate.of(2020, 11, 25),
            0,
            0
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          623.21
        )

        jobSupport.totalEmployeeSalary mustEqual 0.0
        jobSupport.totalEmployersGrant mustEqual 0.0
        jobSupport.totalClosed mustEqual 890.34
        jobSupport.totalGrant mustEqual 890.34
      }

      "scenario-5" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 15)
          ),
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 20),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 4),
            30.3,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 11),
            45.6,
            15
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 18),
            30.5,
            29
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 19),
            LocalDate.of(2020, 11, 25),
            40.12,
            40.12
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          456.66
        )

        jobSupport.totalEmployeeSalary mustEqual 386.84
        jobSupport.totalEmployersGrant mustEqual 357.82
        jobSupport.totalClosed mustEqual 0.0
        jobSupport.totalGrant mustEqual 357.82
      }

      "scenario-6" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 10)
          )
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 11, 4),
            LocalDate.of(2020, 11, 10),
            45.6,
            21.34
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 11),
            LocalDate.of(2020, 11, 17),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24),
            0,
            0
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          700.10
        )

        jobSupport.totalEmployeeSalary mustEqual 248.32
        jobSupport.totalEmployersGrant mustEqual 229.70
        jobSupport.totalClosed mustEqual 466.76
        jobSupport.totalGrant mustEqual 696.46
      }

      "scenario-7" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 9)
          )
        )

        val closedList = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22)
          )
        )

        val pp: List[PayPeriod] = List(
          PayPeriod(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            23,
            10
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            46.56,
            23
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 30),
            0,
            0
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          690.12
        )

        jobSupport.totalEmployeeSalary mustEqual 307.12
        jobSupport.totalEmployersGrant mustEqual 284.09
        jobSupport.totalClosed mustEqual 460.10
        jobSupport.totalGrant mustEqual 744.19
      }

      "scenario-8" in new RegularPayGrantCalculator {

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
          500
        )

        jobSupport.totalEmployeeSalary mustEqual 190.48
        jobSupport.totalEmployersGrant mustEqual 176.20
        jobSupport.totalClosed mustEqual 857.19
        jobSupport.totalGrant mustEqual 1033.39
      }

      "scenario-9" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val temporaryWorkingAgreementPeriods = List(
          TemporaryWorkingAgreementPeriod(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val businessClosedPeriods = List(
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedPeriod(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 25)
          )
        )

        val payPeriods: List[PayPeriod] = List(
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
          payPeriods,
          temporaryWorkingAgreementPeriods,
          businessClosedPeriods,
          PayFrequency.Weekly,
          1000
        )

        jobSupport.totalEmployeeSalary mustEqual 274.74
        jobSupport.totalEmployersGrant mustEqual 254.14
        jobSupport.totalClosed mustEqual 1235.37
        jobSupport.totalGrant mustEqual 1489.51
      }

      "scenario-10" in new RegularPayGrantCalculator {

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
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 21),
            LocalDate.of(2020, 11, 27),
            12,
            4
          )
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
        jobSupport.totalClosed mustEqual 857.19
        jobSupport.totalGrant mustEqual 1180.22
      }

      "scenario-11" in new RegularPayGrantCalculator {

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
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 4),
            LocalDate.of(2020, 11, 10),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 11),
            LocalDate.of(2020, 11, 17),
            0,
            0
          ),
          PayPeriod(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24),
            0,
            0
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          500
        )

        jobSupport.totalEmployeeSalary mustEqual 0.0
        jobSupport.totalEmployersGrant mustEqual 0.0
        jobSupport.totalClosed mustEqual 714.32
        jobSupport.totalGrant mustEqual 714.32
      }
    }
  }
}
