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

import forms.behaviours.CheckboxFieldBehaviours
import generators.Generators
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.data.FormError

class SelectWorkPeriodsFormProviderSpec extends CheckboxFieldBehaviours with Generators with ScalaCheckPropertyChecks {

  val form = new SelectWorkPeriodsFormProvider()()

  ".value" must {

    val fieldName   = "value"
    val requiredKey = "selectWorkPeriods.error.required"

    "bind valid values" in {

      val listGen = for {
        length <- Gen.chooseNum(1, 10)
        list   <- Gen.listOfN(length, periodGen)
      } yield list

      forAll(listGen) { list =>
        val data =
          list.zipWithIndex
            .map(item => s"$fieldName[${item._2}]" -> s"${item._1.startDate.toString}_${item._1.endDate.toString}")
            .toMap
        form.bind(data).get shouldEqual list
      }

    }

    "fail to bind when the answer is invalid" in {
      val data = Map(
        s"$fieldName[0]" -> "invalid value"
      )
      form.bind(data).errors should contain(
        FormError(s"$fieldName[0]", "Text 'invalid value' could not be parsed at index 0")
      )
    }

    "fail to bind when no answers are selected" in {
      val data = Map.empty[String, String]
      form.bind(data).errors should contain(FormError(s"$fieldName", requiredKey))
    }
  }
}
