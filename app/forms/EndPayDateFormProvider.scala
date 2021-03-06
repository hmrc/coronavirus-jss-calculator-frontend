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
import java.time.temporal.ChronoUnit.DAYS

import forms.mappings.Mappings
import javax.inject.Inject
import play.api.data.Form
import play.api.i18n.Messages
import views.ViewUtils.dateToString

class EndPayDateFormProvider @Inject() extends Mappings {

  def apply(lastPayDate: LocalDate, claimStartDate: LocalDate, claimEndDate: LocalDate)(implicit
    messages: Messages
  ): Form[LocalDate] =
    Form(
      "value" -> localDate(
        invalidKey = "endPayDate.error.invalid",
        requiredKey = "endPayDate.error.required",
        Seq(dateToString(lastPayDate))
      ).verifying(
        minDate(claimStartDate, "endPayDate.error.invalid.must.be.on.or.after", dateToString(claimStartDate))
      ).verifying(
        maxDate(claimEndDate, "endPayDate.error.invalid.must.be.on.or.before", dateToString(claimEndDate))
      ).verifying("endPayDate.error.invalid.monthly.days", date => hasValidNofDays(lastPayDate, date))
    )

  private def hasValidNofDays(lastPayDate: LocalDate, endPayDate: LocalDate): Boolean = {
    val days = DAYS.between(lastPayDate, endPayDate)
    days >= 28 && days <= 31
  }
}
