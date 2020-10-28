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

import base.SpecBaseControllerSpecs
import forms.behaviours.DateBehaviours
import models.{BusinessClosedPeriod, ClaimPeriod}
import play.api.data.FormError

class BusinessClosedPeriodsFormProviderSpec extends SpecBaseControllerSpecs {

  val dateBehaviours = new DateBehaviours()
  import dateBehaviours._

  val form = app.injector.instanceOf[BusinessClosedPeriodsFormProvider]

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
      form(List.empty).bind(data).get shouldEqual BusinessClosedPeriod(startDate, endDate)
    }

    "throw form error in case of overlapping periods" in {

      val previousPeriods = List(BusinessClosedPeriod(startDate.minusDays(10), startDate.plusDays(2)))

      form(previousPeriods).bind(data).errors shouldEqual Seq(
        FormError("", "businessClosedPeriods.periods.should.not.overlap")
      )
    }

    "throw form error for dates outside claim start and claim end dates" in {

      val previousPeriods = List(BusinessClosedPeriod(startDate.minusDays(10), startDate.plusDays(2)))

      val startDateInput = form.config.schemeStartDate.minusDays(1)
      val endDateInput   = form.config.schemeEndDate.plusDays(11)
      val data           = Map(
        "startDate.day"   -> startDateInput.getDayOfMonth.toString,
        "startDate.month" -> startDateInput.getMonthValue.toString,
        "startDate.year"  -> startDateInput.getYear.toString,
        "endDate.day"     -> endDateInput.getDayOfMonth.toString,
        "endDate.month"   -> endDateInput.getMonthValue.toString,
        "endDate.year"    -> endDateInput.getYear.toString,
        "addAnother"      -> "false"
      )

      form(previousPeriods).bind(data).errors shouldEqual Seq(
        FormError("startDate", "businessClosedPeriods.startDate.outside.claimPeriod"),
        FormError("endDate", "businessClosedPeriods.endDate.outside.claimPeriod")
      )
    }
  }
}
