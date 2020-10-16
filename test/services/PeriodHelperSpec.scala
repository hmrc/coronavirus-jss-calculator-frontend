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
import models.PayFrequency._
import models.{Period, SupportClaimPeriod}

class PeriodHelperSpec extends SpecBase {

  "Pay Period calculator" when {

    "computing weekly pay periods" must {

      "calculate the correct pay periods when last pay day is before the claim period start date and the pay periods " +
        "do not extend past the claim period end date" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 26),
          Weekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 27),
            LocalDate.of(2020, 11, 2)
          ),
          Period(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 9)
          ),
          Period(
            LocalDate.of(2020, 11, 10),
            LocalDate.of(2020, 11, 16)
          ),
          Period(
            LocalDate.of(2020, 11, 17),
            LocalDate.of(2020, 11, 23)
          ),
          Period(
            LocalDate.of(2020, 11, 24),
            LocalDate.of(2020, 11, 30)
          )
        )
      }

      "calculates the correct pay periods when the last pay day is before the claim period start date" +
        "and the last computed pay period which extends into the next claim period month is ignored" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 25),
          Weekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 26),
            LocalDate.of(2020, 11, 1)
          ),
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 8)
          ),
          Period(
            LocalDate.of(2020, 11, 9),
            LocalDate.of(2020, 11, 15)
          ),
          Period(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 22)
          ),
          Period(
            LocalDate.of(2020, 11, 23),
            LocalDate.of(2020, 11, 29)
          )
        )
      }

      "calculate the correct pay periods when the last pay day is the start of the claim period" in new PeriodHelper {
        val payPeriods: List[Period] =
          getPayPeriods(
            LocalDate.of(2020, 10, 31),
            Weekly,
            SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
          )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 7)
          ),
          Period(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 14)
          ),
          Period(
            LocalDate.of(2020, 11, 15),
            LocalDate.of(2020, 11, 21)
          ),
          Period(
            LocalDate.of(2020, 11, 22),
            LocalDate.of(2020, 11, 28)
          )
        )
      }
    }

    "computing monthly pay periods" must {

      "calculate the correct pay periods when last pay day is the first day of the claim period month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 11, 1),
          Monthly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 30)
          )
        )
      }

      "calculates the correct pay periods when the last pay day is not the last day of the month before the claim period" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 15),
          Monthly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 16),
            LocalDate.of(2020, 11, 14)
          )
        )
      }

      "calculates the correct pay periods when the last pay day is on the last day of the month before the claim period" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 31),
          Monthly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )
        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 30)
          )
        )
      }
    }

    "computing fortnightly pay periods" must {

      "calculate the correct pay periods when last pay day is before the start of the claim period month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 24),
          FortNightly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 25),
            LocalDate.of(2020, 11, 7)
          ),
          Period(
            LocalDate.of(2020, 11, 8),
            LocalDate.of(2020, 11, 21)
          )
        )
      }

      "calculate the correct pay periods when last pay day is before the start of the claim period month and computed " +
        "next pay date is ignored as it is before the claim period start" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 17),
          FortNightly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 14)
          ),
          Period(
            LocalDate.of(2020, 11, 15),
            LocalDate.of(2020, 11, 28)
          )
        )
      }

      "calculate the correct pay periods when last pay day is before the start of the claim period month and the " +
        "computed last pay date is ignored as it falls outside the claim period" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 11, 4),
          FortNightly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 5),
            LocalDate.of(2020, 11, 18)
          )
        )
      }

      "calculate the correct pay periods when last pay day is the penultimate day before the start of the claim period month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 30),
          FortNightly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 13)
          ),
          Period(
            LocalDate.of(2020, 11, 14),
            LocalDate.of(2020, 11, 27)
          )
        )
      }

      "calculate the correct pay periods when last pay day is before the start of the claim period month and the last period is ignored" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 18),
          FortNightly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 19),
            LocalDate.of(2020, 11, 1)
          ),
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 15)
          ),
          Period(
            LocalDate.of(2020, 11, 16),
            LocalDate.of(2020, 11, 29)
          )
        )
      }

    }

    "computing four-weekly pay periods" must {

      "calculate the correct pay periods when last pay day is before the start of the claim period month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 17),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 18),
            LocalDate.of(2020, 11, 14)
          )
        )
      }

      "calculate the correct pay period when last pay day is before the start of the claim period month and one pay " +
        "period occurs before the claim period (ignore)" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 3),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 28)
          )
        )
      }

      "calculate the correct pay period when last pay day is the same as the start of the claim period month and last " +
        "period cuts into the following month (ignore)" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 31),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 1),
            LocalDate.of(2020, 11, 28)
          )
        )
      }

      "calculate 3 the correct pay periods when last pay day is before the start of the claim period month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 11, 2),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 11, 3),
            LocalDate.of(2020, 11, 30)
          )
        )
      }

      "calculate the correct pay periods when last pay day is on the last day of that month" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 30),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 27)
          )
        )
      }

      "calculate the correct pay periods when last pay day is before the start of the claim period month and the next " +
        "pay is computed to be the first day of the claim period" in new PeriodHelper {
        val payPeriods: List[Period] = getPayPeriods(
          LocalDate.of(2020, 10, 4),
          FourWeekly,
          SupportClaimPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 30))
        )

        payPeriods mustBe List(
          Period(
            LocalDate.of(2020, 10, 5),
            LocalDate.of(2020, 11, 1)
          ),
          Period(
            LocalDate.of(2020, 11, 2),
            LocalDate.of(2020, 11, 29)
          )
        )
      }

    }

  }
}
