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

class WeeklySpec extends SpecBase {

  "Regular Pay Calculator" when {

    "computing the JSS-O on weekly freq" should {

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
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 16)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            15,
            7.5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            15,
            7.5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            15,
            7.5
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            37.5,
            15
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 30),
            37.5,
            15
          )
        )

        val jobSupport: JobSupport = calculateJobSupport(
          supportClaimPeriod,
          pp,
          twasList,
          closedList,
          PayFrequency.Weekly,
          300
        )

        jobSupport.totalEmployeeSalary mustEqual 268.60
        jobSupport.totalEmployersGrant mustEqual 248.45
        jobSupport.totalClosed mustEqual 400
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 648.45
      }

      "scenario-2" in new RegularPayGrantCalculator {

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
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 11)
          ),
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 19),
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 1),
            5.7,
            2
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 8),
            40,
            25
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 9),
            LocalDate.of(2020, 11, 15),
            40,
            22
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22),
            40,
            10
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 252.51000000000002
        jobSupport.totalEmployersGrant mustEqual 233.57
        jobSupport.totalClosed mustEqual 533.3333333333334
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 766.9033333333334
      }

      "scenario-3" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 7),
            0,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 14),
            0,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 15),
            LocalDate.of(2020, 11, 21),
            0,
            0
          ),
          PeriodWithHours(
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
        jobSupport.totalClosed mustEqual 1401.493333333333
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 1401.493333333333
      }

      "scenario-4" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 21)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 4),
            0,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 11),
            0,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 18),
            0,
            0
          ),
          PeriodWithHours(
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
        jobSupport.totalClosed mustEqual 890.3
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 890.3
      }

      "scenario-5" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 15)
          ),
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 20),
            LocalDate.of(2020, 11, 30)
          )
        )

        val closedList = List(
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 29),
            LocalDate.of(2020, 11, 4),
            30.3,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 11),
            45.6,
            15
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 12),
            LocalDate.of(2020, 11, 18),
            30.5,
            29
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 486.86
        jobSupport.totalEmployersGrant mustEqual 450.35
        jobSupport.totalClosed mustEqual 0
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 450.35
      }

      "scenario-6" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 10)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 18),
            LocalDate.of(2020, 11, 24)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 11, 4),
            LocalDate.of(2020, 11, 10),
            45.6,
            21.34
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 11),
            LocalDate.of(2020, 11, 17),
            0,
            0
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 248.33
        jobSupport.totalEmployersGrant mustEqual 229.70
        jobSupport.totalClosed mustEqual 466.73333333333335
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 696.4333333333334
      }

      "scenario-7" in new RegularPayGrantCalculator {

        val supportClaimPeriod = SupportClaimPeriod(
          LocalDate.of(2020, 11, 1),
          LocalDate.of(2020, 11, 30)
        )

        val twasList = List(
          TemporaryWorkingAgreementWithDates(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 9)
          )
        )

        val closedList = List(
          BusinessClosedWithDates(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            23,
            10
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            46.56,
            23
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            0,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            0,
            0
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 307.13
        jobSupport.totalEmployersGrant mustEqual 284.09000000000003
        jobSupport.totalClosed mustEqual 460.08
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 744.1700000000001
      }

      "scenario-8" in new RegularPayGrantCalculator {

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
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 6),
            6,
            6
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 13),
            12,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 14),
            LocalDate.of(2020, 11, 20),
            24,
            12
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 190.5
        jobSupport.totalEmployersGrant mustEqual 176.20
        jobSupport.totalClosed mustEqual 857.1428571428571
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 1033.3428571428572
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
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 6),
            6,
            6
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 13),
            12,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 14),
            LocalDate.of(2020, 11, 20),
            24,
            12
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 274.0
        jobSupport.totalEmployersGrant mustEqual 253.44
        jobSupport.totalClosed mustEqual 1232.8799999999999
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 1486.32
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
            LocalDate.of(2020, 11, 25)
          )
        )

        val pp: List[PeriodWithHours] = List(
          PeriodWithHours(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 6),
            6,
            6
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 7),
            LocalDate.of(2020, 11, 13),
            12,
            0
          ),
          PeriodWithHours(
            LocalDate.of(2020, 11, 14),
            LocalDate.of(2020, 11, 20),
            24,
            0
          ),
          PeriodWithHours(
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

        jobSupport.totalEmployeeSalary mustEqual 349.24
        jobSupport.totalEmployersGrant mustEqual 323.03999999999996
        jobSupport.totalClosed mustEqual 857.1428571428571
        jobSupport.totalEmployersGrant + jobSupport.totalClosed mustEqual 1180.182857142857
      }
    }
  }
}
