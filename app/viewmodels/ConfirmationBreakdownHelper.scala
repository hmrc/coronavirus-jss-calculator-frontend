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

package viewmodels

import com.google.inject.{Inject, Singleton}
import models.GrantForPeriod
import models.PayFrequency.Monthly
import play.api.i18n.Messages
import views.ViewUtils._

@Singleton
class ConfirmationBreakdownHelper @Inject()() {

  def breakdownH3(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String =
    messages(
      "confirmation.breakdown.h3",
      dateToStringWithoutYear(grantForPeriod.period.startDate),
      dateToString(grantForPeriod.period.endDate),
      if (grantForPeriod.payFrequency == Monthly) {
        grantForPeriod.daysInFrequency
      } else if (grantForPeriod.isPartialPayPeriod) grantForPeriod.daysInPeriod
      else grantForPeriod.daysInFrequency.-(1)
    )

  def totalPayForTheDaysWorked(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String =
    if (grantForPeriod.isPartialPayPeriod) {
      messages(
        "confirmation.breakdown.h3.p1.l1.partial",
        grantForPeriod.referencePay,
        grantForPeriod.daysInPeriod,
        grantForPeriod.daysInFrequency,
        grantForPeriod.referencePay * grantForPeriod.daysInPeriod / grantForPeriod.daysInFrequency
      )
    } else {
      messages(
        "confirmation.breakdown.h3.p1.l1",
        grantForPeriod.referencePay,
      )
    }

  def referencePayCapForTheDaysWorked(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String =
    if (grantForPeriod.isPartialPayPeriod) {
      messages(
        "confirmation.breakdown.h3.p1.l2.partial",
        grantForPeriod.referencePayCap,
        grantForPeriod.daysInPeriod,
        grantForPeriod.referencePayCap * grantForPeriod.daysInPeriod
      )
    } else {
      messages(
        "confirmation.breakdown.h3.p1.l2",
        grantForPeriod.referencePayCap,
      )
    }

  def hoursNotWorked(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String =
    messages(
      "confirmation.breakdown.h3.p1.l4",
      grantForPeriod.period.usualHours,
      grantForPeriod.period.actualHours,
      grantForPeriod.period.usualHours - grantForPeriod.period.actualHours
    )

  def cappedGrant(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String = {
    val hoursNotWorked = grantForPeriod.period.usualHours - grantForPeriod.period.actualHours
    val finalGrant = grantForPeriod.adjustedReferencePay * hoursNotWorked / grantForPeriod.period.usualHours
    messages(
      "confirmation.breakdown.h3.p1.grant",
      grantForPeriod.adjustedReferencePay,
      hoursNotWorked,
      grantForPeriod.period.usualHours,
      finalGrant,
    )
  }

  def eligibility(grantForPeriod: GrantForPeriod)(implicit messages: Messages): String = {
    val hoursNotWorked = grantForPeriod.period.usualHours - grantForPeriod.period.actualHours
    val grant = grantForPeriod.adjustedReferencePay * hoursNotWorked / grantForPeriod.period.usualHours
    messages(
      "confirmation.breakdown.h3.p1.grant.eligible",
      grant,
      3,
      (grant / 3).formatted("%.2f")
    )
  }
}
