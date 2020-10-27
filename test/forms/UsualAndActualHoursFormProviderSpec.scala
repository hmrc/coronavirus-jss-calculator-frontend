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

import forms.behaviours.DecimalFieldBehaviours
import play.api.data.FormError

class UsualAndActualHoursFormProviderSpec extends DecimalFieldBehaviours {

  val form = new UsualAndActualHoursFormProvider()()

  ".usualHours" must {

    val usualHours = "usualHours"

    val validDataGenerator = decimals

    behave like fieldThatBindsValidData(
      form,
      usualHours,
      validDataGenerator
    )

    behave like decimalField(
      form,
      usualHours,
      nonNumericError = FormError(usualHours, "usualHours.error.nonNumeric")
    )

    behave like mandatoryField(
      form,
      usualHours,
      requiredError = FormError(usualHours, "usualHours.error.required")
    )
  }

  ".actualHours" must {

    val actualHours = "actualHours"

    val validDataGenerator = decimals

    behave like fieldThatBindsValidData(
      form,
      actualHours,
      validDataGenerator
    )

    behave like decimalField(
      form,
      actualHours,
      nonNumericError = FormError(actualHours, "actualHours.error.nonNumeric")
    )

    behave like mandatoryField(
      form,
      actualHours,
      requiredError = FormError(actualHours, "actualHours.error.required")
    )
  }

  "errors should be shown if actual hours is more than usual hours" in {

    val result = form.bind(Map("usualHours" -> "20.00", "actualHours" -> "21.00"))
    result.errors shouldEqual Seq(FormError("", "actualHours.cannot.be.morethan.usualHours", Seq.empty))
  }
}
