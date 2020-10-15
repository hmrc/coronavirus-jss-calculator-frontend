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
          LocalDate.of(2020, 10, 27),
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
          LocalDate.of(2020, 10, 26),
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
            LocalDate.of(2020, 11, 1),
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
            LocalDate.of(2020, 11, 1),
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
            LocalDate.of(2020, 10, 15),
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
            LocalDate.of(2020, 10, 31),
            LocalDate.of(2020, 11, 30)
          )
        )
      }
    }

  }
}
