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
import models.BusinessClosedPeriods

class BusinessClosedPeriodsFormProviderSpec extends DateBehaviours {

  val form = new BusinessClosedPeriodsFormProvider()()

  "form" should {

    val startDate = LocalDate.of(2020, 10, 20)
    val endDate = startDate.plusDays(10)
    "bind valid values" in {
      val data = Map(
        "startDate.day"   -> startDate.getDayOfMonth.toString,
        "startDate.month" -> startDate.getMonthValue.toString,
        "startDate.year"  -> startDate.getYear.toString,
        "endDate.day"     -> endDate.getDayOfMonth.toString,
        "endDate.month"   -> endDate.getMonthValue.toString,
        "endDate.year"    -> endDate.getYear.toString
      )

      form.bind(data).get shouldEqual BusinessClosedPeriods(startDate, endDate)
    }
  }
}
