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

import forms.mappings.Mappings
import javax.inject.Inject
import models.UsualAndActualHours
import play.api.data.Form
import play.api.data.Forms._

class UsualAndActualHoursFormProvider @Inject() extends Mappings {

  def apply(): Form[UsualAndActualHours] =
    Form(
      mapping(
        "usualHours"  -> double("usualHours.error.required", "usualHours.error.nonNumeric")
          .verifying(greaterThan(0.0, "usualHours.error.min")),
        "actualHours" -> double("actualHours.error.required", "actualHours.error.nonNumeric")
          .verifying(greaterThanOrEquals(0.0, "actualHours.error.min"))
      )(UsualAndActualHours.apply)(UsualAndActualHours.unapply)
        .verifying("actualHours.cannot.be.morethan.usualHours", uaHours => uaHours.actualHours <= uaHours.usualHours)
    )
}
