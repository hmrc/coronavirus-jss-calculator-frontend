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

import play.api.libs.json._
import utils.ValueClassFormat

case class Amount(value: BigDecimal)

object Amount {
  implicit val format: Format[Amount] = ValueClassFormat.format(value => Amount.apply(BigDecimal(value)))(_.value)

  implicit class Defaulted(maybeAmount: Option[Amount]) {
    def defaulted: Amount = maybeAmount.fold(Amount(0.0))(v => v)
  }

  implicit class FromDouble(value: Double) {
    def toAmount: Amount = Amount(value)
  }

  implicit class FromBigDecimal(value: BigDecimal) {
    def toAmount: Amount = Amount(value)
  }
}
