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

import play.api.libs.json.{Json, OFormat}

final case class GrantForPeriod(
  period: PeriodWithHours,
  amount: BigDecimal,
  daysInPeriod: Int,
  daysInFrequency: Int,
  referencePayCap: Double,
  adjustedReferencePay: Double,
  referencePay: BigDecimal,
  payFrequency: PayFrequency,
  isPartialPayPeriod: Boolean,
)

object GrantForPeriod {
  implicit val format: OFormat[GrantForPeriod] = Json.format[GrantForPeriod]
}

final case class Grant(
  grantForPeriods: List[GrantForPeriod],
  referencePay: BigDecimal,
  isEligible: Boolean,
  totalGrant: BigDecimal
)

object Grant {
  implicit val format: OFormat[Grant] = Json.format[Grant]
}
