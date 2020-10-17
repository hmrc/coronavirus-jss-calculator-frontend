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
import models.{PayFrequency, Period, SupportClaimPeriod}

class RegularPayGrantCalculatorSpec extends SpecBase {

  "Regular Pay Calculator" when {

    "computing the grant for weekly frequencies" should {

      "correctly compute the grant for default scenario" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 1),
            5.7,
            2
          ),
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 8),
            40,
            25
          ),
          Period(
            LocalDate.of(2020, 11, 9),
            LocalDate.of(2020, 11, 15),
            40,
            22
          ),
          Period(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22),
            40,
            10
          ),
          Period(
            LocalDate.of(2020, 11, 23),
            LocalDate.of(2020, 11, 29),
            40,
            15
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          350,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 267.49
      }

      "correctly compute the grant for partial period at the beginning of the month - STWA agreed for all pay periods" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            15,
            7.5
          ),
          Period(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            37.5,
            15
          ),
          Period(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            37.5,
            15
          ),
          Period(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            37.5,
            15
          ),
          Period(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 20),
            37.5,
            15
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          300,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 254.29
      }

      "correctly compute the grant for Partial period at the beginning of the month - STWA agreed for all pay periods - " +
        "Last pay period cuts across to December" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 1),
            5.7,
            2
          ),
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 8),
            40,
            25
          ),
          Period(
            LocalDate.of(2020, 11, 9),
            LocalDate.of(2020, 11, 15),
            40,
            22
          ),
          Period(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22),
            40,
            10
          ),
          Period(
            LocalDate.of(2020, 11, 23),
            LocalDate.of(2020, 11, 29),
            40,
            15
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          350,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )

        calculatedGrantForEachPeriod.totalGrant mustEqual 267.49
      }

      "correctly compute the grant for Ref pay capped" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 30),
            LocalDate.of(2020, 11, 5),
            15,
            7.5
          ),
          Period(
            LocalDate.of(2020, 11, 6),
            LocalDate.of(2020, 11, 12),
            37.5,
            15
          ),
          Period(
            LocalDate.of(2020, 11, 13),
            LocalDate.of(2020, 11, 19),
            37.5,
            15
          ),
          Period(
            LocalDate.of(2020, 11, 20),
            LocalDate.of(2020, 11, 26),
            37.5,
            15
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          750,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 519.50
      }

      "correctly compute the grant for eligibility not met. Less than 33% " in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2),
            15,
            0
          ),
          Period(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9),
            37.5,
            7.5
          ),
          Period(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16),
            37.5,
            7.5
          ),
          Period(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23),
            37.5,
            7.5
          ),
          Period(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 30),
            37.5,
            7.5
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          250,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 290.49
      }

      "correctly compute the grant for select only one pay period within the claim period" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 1),
            LocalDate.of(2020, 11, 7),
            37.5,
            15
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          350,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 70
      }

      "correctly compute the grant for November claim - Select three of the pay periods for the calculation" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 10, 1),
            LocalDate.of(2020, 11, 7),
            40,
            14
          ),
          Period(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 14),
            40,
            14
          ),
          Period(
            LocalDate.of(2020, 11, 22),
            LocalDate.of(2020, 11, 28),
            40,
            14
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          400,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 260.01
      }

      "correctly compute the grant for November claim - Staggered STWA covering 2 pay periods" in new RegularPayGrantCalculator {

        val periods = List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 7),
            50,
            30
          ),
          Period(
            LocalDate.of(2020, 11, 22),
            LocalDate.of(2020, 11, 28),
            50,
            30
          )
        )

        val calculatedGrantForEachPeriod = calculateRegularPayGrant(
          periods,
          670,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30)),
          PayFrequency.Weekly
        )
        calculatedGrantForEachPeriod.totalGrant mustEqual 178.66
      }
    }
  }
}
