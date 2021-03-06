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

import forms.behaviours.BigDecimalFieldBehaviours
import play.api.data.FormError

class RegularPayAmountFormProviderSpec extends BigDecimalFieldBehaviours {

  val requiredKey = "regularPayAmount.error.required"
  val invalidKey  = "regularPayAmount.error.nonNumeric"

  val form = new RegularPayAmountFormProvider()()

  ".value" must {

    val fieldName = "value"

    behave like bigDecimalField(
      form,
      fieldName,
      error = FormError(fieldName, invalidKey)
    )

    "not allow zero for pay" in {
      form.bind(Map("value" -> "0.0")).errors shouldBe Seq(
        FormError("value", "regularPayAmount.error.negative")
      )
    }

    "bind otherwise valid values with a £" in {
      forAll(positiveBigDecimalsWith2dp -> "bigDecimals") { bigDecimal: BigDecimal =>
        val result = form.bind(Map(fieldName -> s"£$bigDecimal")).apply(fieldName)
        result.errors shouldEqual Seq.empty
      }
    }

    behave like mandatoryField(
      form,
      fieldName,
      requiredError = FormError(fieldName, requiredKey)
    )
  }
}
