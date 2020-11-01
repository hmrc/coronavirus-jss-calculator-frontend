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

import config.FrontendAppConfig
import forms.mappings.Mappings
import javax.inject.Inject
import models.BusinessClosedPeriod
import play.api.data.Form
import play.api.data.Forms.mapping

class BusinessClosedPeriodsFormProvider @Inject() (val config: FrontendAppConfig) extends Mappings with FormHelper {

  def apply(
    previousBCPeriods: Seq[BusinessClosedPeriod]
  ): Form[BusinessClosedPeriod] =
    Form(
      mapping(
        "startDate"  -> localDate(
          invalidKey = "businessClosedPeriods.error.invalid.start",
          requiredKey = "businessClosedPeriods.error.invalid.start"
        ).verifying(
          "businessClosedPeriods.startDate.outside.claimPeriod",
          date => isDateWithinSchemeDates(date)
        ),
        "endDate"    -> localDateAfterAnother(
          otherField = "startDate",
          otherPeriods = previousBCPeriods,
          minimumDaysBetween = 6,
          invalidKey = "businessClosedPeriods.error.invalid.end",
          requiredKey = "businessClosedPeriods.error.invalid.end",
          mustBeAfterKey = "businessClosedPeriods.endDate.must.be.after.startDate",
          daysBetweenKey = "businessClosedPeriods.period.shouldbe.minimum.7.days",
          mustNotOverlapKey = "businessClosedPeriods.periods.should.not.overlap"
        ).verifying(
          "businessClosedPeriods.endDate.outside.claimPeriod",
          date => isDateWithinSchemeDates(date)
        ),
        "addAnother" -> boolean(
          requiredKey = "businessClosedPeriods.addAnother.error.required"
        )
      )(BusinessClosedPeriod.apply)(BusinessClosedPeriod.unapply)
    )
}
