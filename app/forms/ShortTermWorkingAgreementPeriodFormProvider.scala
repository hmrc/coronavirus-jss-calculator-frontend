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
import models.TemporaryWorkingAgreementPeriod
import play.api.data.Form
import play.api.data.Forms._

class ShortTermWorkingAgreementPeriodFormProvider @Inject() (val config: FrontendAppConfig)
    extends Mappings
    with FormHelper {

  def apply(
    previousTWAPeriods: Seq[TemporaryWorkingAgreementPeriod]
  ): Form[TemporaryWorkingAgreementPeriod] =
    Form(
      mapping(
        "startDate"  -> localDate(
          invalidKey = "shortTermWorkingAgreementPeriod.error.invalid.start",
          requiredKey = "shortTermWorkingAgreementPeriod.error.required.start"
        ).verifying(
          "shortTermWorkingAgreementPeriod.startDate.outside.claimPeriod",
          date => isDateWithinSchemeDates(date)
        ),
        "endDate"    -> localDateAfterAnother(
          otherField = "startDate",
          otherPeriods = previousTWAPeriods,
          minimumDaysBetween = 6,
          invalidKey = "shortTermWorkingAgreementPeriod.error.invalid.end",
          requiredKey = "shortTermWorkingAgreementPeriod.error.invalid.end",
          mustBeAfterKey = "shortTermWorkingAgreementPeriod.endDate.must.be.after.startDate",
          daysBetweenKey = "shortTermWorkingAgreementPeriod.period.shouldbe.minimum.7.days",
          mustNotOverlapKey = "shortTermWorkingAgreementPeriod.periods.should.not.overlap"
        ).verifying(
          "shortTermWorkingAgreementPeriod.endDate.outside.claimPeriod",
          date => isDateWithinSchemeDates(date)
        ),
        "addAnother" -> boolean(
          requiredKey = "shortTermWorkingAgreementPeriod.addAnother.error.required"
        )
      )(TemporaryWorkingAgreementPeriod.apply)(TemporaryWorkingAgreementPeriod.unapply)
    )
}
