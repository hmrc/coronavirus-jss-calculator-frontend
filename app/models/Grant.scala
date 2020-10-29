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

package models

import julienrf.json.derived
import models.PeriodGrant.OpenPeriodGrant
import play.api.libs.json.{Json, OFormat}

sealed trait PeriodGrant

object PeriodGrant {
  final case class OpenPeriodGrant(
    period: PayPeriod,
    grant: BigDecimal, //TODO: change to Double
    employerContribution: Double,
    daysInPeriod: Int,
    daysInFrequency: Int,
    referencePayCap: BigDecimal,
    adjustedReferencePay: Double,
    payFrequency: PayFrequency,
    isPartialPayPeriod: Boolean,
    hoursNotWorked: Int
  ) extends PeriodGrant

  object OpenPeriodGrant {
    implicit val format: OFormat[OpenPeriodGrant] = Json.format[OpenPeriodGrant]
  }

  final case class ClosedPeriodGrant(
    period: PayPeriod,
    amount: BigDecimal, //TODO: change to Double
    employerContribution: Double,
    daysInPeriod: Int,
    daysInFrequency: Int,
    referencePayCap: BigDecimal,
    adjustedReferencePay: Double,
    payFrequency: PayFrequency,
    isPartialPayPeriod: Boolean,
    hoursNotWorked: Int
  ) extends PeriodGrant

  object ClosedPeriodGrant {
    implicit val format: OFormat[ClosedPeriodGrant] = Json.format[ClosedPeriodGrant]
  }

  implicit val format: OFormat[PeriodGrant] = derived.oformat[PeriodGrant]()
}

final case class Grant(
  grantForPeriods: List[OpenPeriodGrant],
  referencePay: BigDecimal,
  isEligible: Boolean,
  totalGrant: Double
//  ,
//  totalEmployerContribution: Double
)

object Grant {
  implicit val format: OFormat[Grant] = Json.format[Grant]
}
