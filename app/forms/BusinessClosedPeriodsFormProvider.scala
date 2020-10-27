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

import forms.mappings.Mappings
import javax.inject.Inject
import models.{BusinessClosedWithDates, SupportClaimPeriod}
import play.api.data.Form
import play.api.data.Forms.mapping

class BusinessClosedPeriodsFormProvider @Inject() extends Mappings {

  def apply(
    previousBCPeriods: Seq[BusinessClosedWithDates],
    claimPeriod: SupportClaimPeriod
  ): Form[BusinessClosedWithDates] =
    Form(
      mapping(
        "startDate" -> localDate(
          invalidKey = "businessClosedPeriods.error.invalid",
          allRequiredKey = "businessClosedPeriods.error.required.all",
          twoRequiredKey = "businessClosedPeriods.error.required.two",
          requiredKey = "businessClosedPeriods.error.required"
        ).verifying(
          "businessClosedPeriods.startDate.outside.claimPeriod",
          date => isDateWithInClaim(date, claimPeriod)
        ),
        "endDate"   -> localDate(
          invalidKey = "businessClosedPeriods.error.invalid",
          allRequiredKey = "businessClosedPeriods.error.required.all",
          twoRequiredKey = "businessClosedPeriods.error.required.two",
          requiredKey = "businessClosedPeriods.error.required"
        )
      )(BusinessClosedWithDates.apply)(BusinessClosedWithDates.unapply)
        .verifying(
          "businessClosedPeriods.endDate.must.be.after.startDate",
          bcp => bcp.endDate.compareTo(bcp.startDate) > 0
        )
        .verifying("businessClosedPeriods.periods.should.not.overlap", bcp => !isIntersecting(previousBCPeriods, bcp))
    )

  private def isDateWithInClaim(date: LocalDate, claimPeriod: SupportClaimPeriod) =
    (date.isEqual(claimPeriod.startDate) || date.isAfter(claimPeriod.startDate)) && date.isBefore(claimPeriod.endDate)

  private def isIntersecting(previousPeriods: Seq[BusinessClosedWithDates], newPeriod: BusinessClosedWithDates) =
    previousPeriods.exists(p =>
      isDateInteractsPeriod(newPeriod.startDate, p) || isDateInteractsPeriod(newPeriod.endDate, p)
    )

  private def isDateInteractsPeriod(date: LocalDate, period: BusinessClosedWithDates) =
    date.compareTo(period.startDate) >= 0 && date.compareTo(period.endDate) <= 0;
}
