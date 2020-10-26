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

import java.time.{LocalDate, ZoneOffset}

import base.SpecBaseControllerSpecs
import forms.behaviours.DateBehaviours
import play.api.data.FormError

class EndPayDateFormProviderSpec extends SpecBaseControllerSpecs {

  val dateBehaviours = new DateBehaviours()
  import dateBehaviours._

  def form(lastPayDate: LocalDate) = new EndPayDateFormProvider()(lastPayDate)

  val lastPayDate = LocalDate.of(2020, 10, 31)

  ".value" should {

    "bind valid values" in {

      val validData = datesBetween(
        min = LocalDate.of(2000, 11, 1),
        max = LocalDate.now(ZoneOffset.UTC)
      )

      behave like dateField(form(lastPayDate), "value", validData)
    }

    "not bind any dates earlier than LastPayDate" in {
      val inputDate = lastPayDate.minusDays(1)
      val data      = Map(
        s"$value.day"   -> inputDate.getDayOfMonth.toString,
        s"$value.month" -> inputDate.getMonthValue.toString,
        s"$value.year"  -> inputDate.getYear.toString
      )

      val result = form(lastPayDate).bind(data)
      result.errors shouldBe List(
        FormError("value", List("endPayDate.error.invalid.must.be.after"), Array("31 October 2020"))
      )
    }

    "fail to bind when no answers are selected" in {
      behave like mandatoryDateField(form(lastPayDate), "value", "endPayDate.error.required.all")
    }
  }
}
