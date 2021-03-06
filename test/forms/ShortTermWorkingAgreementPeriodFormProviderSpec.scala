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
import models.TemporaryWorkingAgreementPeriod
import play.api.data.FormError

class ShortTermWorkingAgreementPeriodFormProviderSpec extends SpecBaseControllerSpecs {

  val dateBehaviours = new DateBehaviours()
  import dateBehaviours._

  val form = app.injector.instanceOf[ShortTermWorkingAgreementPeriodFormProvider]

  "form" should {

    val startDate = LocalDate.of(2020, 11, 1)
    val endDate   = startDate.plusDays(10)
    val data      = Map(
      "startDate.day"   -> startDate.getDayOfMonth.toString,
      "startDate.month" -> startDate.getMonthValue.toString,
      "startDate.year"  -> startDate.getYear.toString,
      "endDate.day"     -> endDate.getDayOfMonth.toString,
      "endDate.month"   -> endDate.getMonthValue.toString,
      "endDate.year"    -> endDate.getYear.toString,
      "addAnother"      -> "true"
    )

    "bind valid values" in {
      form(List.empty).bind(data).get shouldEqual TemporaryWorkingAgreementPeriod(
        startDate,
        endDate,
        true
      )
    }

    "throw form error in case of overlapping periods" in {

      val previousPeriods = List(TemporaryWorkingAgreementPeriod(startDate.minusDays(10), startDate.plusDays(2)))

      form(previousPeriods).bind(data).errors shouldEqual Seq(
        FormError("endDate", "shortTermWorkingAgreementPeriod.periods.should.not.overlap")
      )
    }

    "throw a form error for a start date before the scheme start date" in {

      val startDateInput = form.config.schemeStartDate.minusDays(1)

      val data = Map(
        "startDate.day"   -> startDateInput.getDayOfMonth.toString,
        "startDate.month" -> startDateInput.getMonthValue.toString,
        "startDate.year"  -> startDateInput.getYear.toString,
        "endDate.day"     -> endDate.getDayOfMonth.toString,
        "endDate.month"   -> endDate.getMonthValue.toString,
        "endDate.year"    -> endDate.getYear.toString,
        "addAnother"      -> "false"
      )

      form(Nil).bind(data).errors shouldEqual Seq(
        FormError("startDate", "shortTermWorkingAgreementPeriod.startDate.outside.claimPeriod")
      )
    }

    "throw a form error for an end date after the scheme end date" in {

      val endDateInput = form.config.schemeEndDate.plusDays(1)

      val data = Map(
        "startDate.day"   -> startDate.getDayOfMonth.toString,
        "startDate.month" -> startDate.getMonthValue.toString,
        "startDate.year"  -> startDate.getYear.toString,
        "endDate.day"     -> endDateInput.getDayOfMonth.toString,
        "endDate.month"   -> endDateInput.getMonthValue.toString,
        "endDate.year"    -> endDateInput.getYear.toString,
        "addAnother"      -> "false"
      )

      form(Nil).bind(data).errors shouldEqual Seq(
        FormError("endDate", "shortTermWorkingAgreementPeriod.endDate.outside.claimPeriod")
      )
    }
  }
}
