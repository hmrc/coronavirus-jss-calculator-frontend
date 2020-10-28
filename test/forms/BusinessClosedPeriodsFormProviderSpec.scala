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

package forms

import java.time.LocalDate

import forms.behaviours.DateBehaviours
import models.{BusinessClosedWithDates, ClaimPeriod}
import play.api.data.FormError

class BusinessClosedPeriodsFormProviderSpec extends DateBehaviours {

  val form = new BusinessClosedPeriodsFormProvider()

  val claimPeriod = ClaimPeriod.Nov2020.supportClaimPeriod
  val startDate   = LocalDate.of(2020, 11, 1)
  val endDate     = startDate.plusDays(10)

  val data = Map(
    "startDate.day"   -> startDate.getDayOfMonth.toString,
    "startDate.month" -> startDate.getMonthValue.toString,
    "startDate.year"  -> startDate.getYear.toString,
    "endDate.day"     -> endDate.getDayOfMonth.toString,
    "endDate.month"   -> endDate.getMonthValue.toString,
    "endDate.year"    -> endDate.getYear.toString,
    "addAnother"      -> "false"
  )

  "form" should {

    "bind valid values" in {
      form(List.empty, claimPeriod).bind(data).get shouldEqual BusinessClosedWithDates(startDate, endDate)
    }

    "throw form error in case of overlapping periods" in {

      val previousPeriods = List(BusinessClosedWithDates(startDate.minusDays(10), startDate.plusDays(2)))

      form(previousPeriods, claimPeriod).bind(data).errors shouldEqual Seq(
        FormError("", "businessClosedPeriods.periods.should.not.overlap")
      )
    }
  }
}
