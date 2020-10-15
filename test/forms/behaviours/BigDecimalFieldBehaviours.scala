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

package forms.behaviours

import play.api.data.{Form, FormError}

trait BigDecimalFieldBehaviours extends FieldBehaviours {

  def bigDecimalField(form: Form[_], fieldName: String, error: FormError): Unit = {

    "bind all big decimal values with decimal places <= 2" in {
      forAll(positiveBigDecimalsWith2dp -> "bigDecimals") { bigDecimal: BigDecimal =>
        val result = form.bind(Map(fieldName -> bigDecimal.toString)).apply(fieldName)
        result.errors shouldEqual Seq.empty
      }
    }

    "not bind all big decimal values with decimal places > 2" in {
      forAll(positiveBigDecimalsWithMoreThan2dp -> "bigDecimals") { bigDecimal: BigDecimal =>
        val result = form.bind(Map(fieldName -> bigDecimal.toString)).apply(fieldName)
        result.errors shouldEqual Seq(FormError("value", "amount.error.max.2.decimals"))
      }
    }

    "not bind non-numeric numbers" in {

      forAll(nonNumerics -> "nonNumeric") { nonNumeric =>
        val result = form.bind(Map(fieldName -> nonNumeric)).apply(fieldName)
        result.errors shouldEqual Seq(error)
      }
    }
  }
}
