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
import play.api.data.FormError

class LastPayDateFormProviderSpec extends SpecBaseControllerSpecs {

  val dateBehaviours = new DateBehaviours()
  import dateBehaviours._

  def form(firstDateOfClaim: LocalDate) = new LastPayDateFormProvider()(firstDateOfClaim)

  ".value" should {

    val firstDateOfClaim = LocalDate.of(2020, 11, 1)

    "bind valid values" in {

      val validData = datesBetween(
        min = LocalDate.of(2020, 10, 1),
        max = firstDateOfClaim.minusDays(1)
      )

      behave like dateField(form(firstDateOfClaim), "value", validData)
    }

    "not bind any dates later than first day of the claim" in {
      val inputDate = firstDateOfClaim.plusDays(1)
      val data = Map(
        s"$value.day"   -> inputDate.getDayOfMonth.toString,
        s"$value.month" -> inputDate.getMonthValue.toString,
        s"$value.year"  -> inputDate.getYear.toString
      )

      val result = form(firstDateOfClaim).bind(data)
      result.errors shouldBe List(FormError("value", List("lastPayDate.error.invalid.must.be.before"), Array("1 November 2020")))
    }

    "fail to bind when no answers are selected" in {
      behave like mandatoryDateField(form(firstDateOfClaim), "value", "lastPayDate.error.required.all")
    }
  }
}
