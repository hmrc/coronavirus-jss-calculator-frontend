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
import java.time.temporal.ChronoUnit

import forms.mappings.Mappings
import javax.inject.Inject
import models.{SupportClaimPeriod, TemporaryWorkingAgreementWithDates}
import play.api.data.Form
import play.api.data.Forms._

class ShortTermWorkingAgreementPeriodFormProvider @Inject() extends Mappings {

  def apply(
    previousTWAPeriods: Seq[TemporaryWorkingAgreementWithDates],
    claimPeriod: SupportClaimPeriod
  ): Form[TemporaryWorkingAgreementWithDates] =
    Form(
      mapping(
        "startDate"  -> localDate(
          invalidKey = "shortTermWorkingAgreementPeriod.error.invalid",
          requiredKey = "shortTermWorkingAgreementPeriod.error.required"
        ).verifying(
          "shortTermWorkingAgreementPeriod.startDate.outside.claimPeriod",
          date => isDateWithInClaim(date, claimPeriod)
        ),
        "endDate"    -> localDate(
          invalidKey = "shortTermWorkingAgreementPeriod.error.invalid",
          requiredKey = "shortTermWorkingAgreementPeriod.error.required"
        ),
        "addAnother" -> boolean(
          requiredKey = "shortTermWorkingAgreementPeriod.addAnother.error.required"
        )
      )(TemporaryWorkingAgreementWithDates.apply)(TemporaryWorkingAgreementWithDates.unapply)
        .verifying(
          "shortTermWorkingAgreementPeriod.endDate.must.be.after.startDate",
          swa => swa.endDate.compareTo(swa.startDate) > 0
        )
        .verifying(
          "shortTermWorkingAgreementPeriod.periods.should.not.overlap",
          swa => !isIntersecting(previousTWAPeriods, swa)
        )
        .verifying(
          "shortTermWorkingAgreementPeriod.period.shouldbe.minimum.7.days",
          swa => ChronoUnit.DAYS.between(swa.startDate, swa.endDate) >= 6 //endDate is exclusive for 'between' so '6'
        )
    )

  private def isDateWithInClaim(date: LocalDate, claimPeriod: SupportClaimPeriod) =
    (date.isEqual(claimPeriod.startDate) || date.isAfter(claimPeriod.startDate)) && date.isBefore(claimPeriod.endDate)

  private def isIntersecting(
    previousPeriods: Seq[TemporaryWorkingAgreementWithDates],
    newPeriod: TemporaryWorkingAgreementWithDates
  ) =
    previousPeriods.exists(p =>
      isDateInteractsPeriod(newPeriod.startDate, p) || isDateInteractsPeriod(newPeriod.endDate, p)
    )

  private def isDateInteractsPeriod(date: LocalDate, period: TemporaryWorkingAgreementWithDates) =
    date.compareTo(period.startDate) >= 0 && date.compareTo(period.endDate) <= 0;
}
