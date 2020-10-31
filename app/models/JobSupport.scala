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

import play.api.libs.json.{Format, Json}
import utils.MoneyUtils.round

final case class OpenJobSupport(
  numberOfTemporaryWorkingDaysInPayPeriod: Int,
  usualHours: Double,
  actualHours: Double,
  salary: Double,
  grant: Double
)

object OpenJobSupport {
  implicit val format: Format[OpenJobSupport] = Json.format
  val zeroFinancialSupport: OpenJobSupport    = OpenJobSupport(0, 0.0, 0.0, 0.0, 0.0)
}

final case class ClosedJobSupport(
  numberOfClosedDaysInPayPeriod: Int,
  grant: Double
)

object ClosedJobSupport {
  implicit val format: Format[ClosedJobSupport] = Json.format
  val zeroFinancialSupport: ClosedJobSupport    = ClosedJobSupport(0, 0.0)
}

final case class JobSupport(
  supportBreakdowns: List[SupportBreakdown],
  referenceSalary: Double
)

object JobSupport {

  implicit class JobSupportOps(private val jobSupport: JobSupport) {

    def totalActualHours: Double = round(
      jobSupport.supportBreakdowns.map(supportBreakdown => supportBreakdown.open.actualHours).sum
    )

    def totalUsualHours: Double = round(
      jobSupport.supportBreakdowns.map(supportBreakdown => supportBreakdown.open.usualHours).sum
    )

    def isIneligible: Boolean = (totalActualHours / totalUsualHours) < 0.20

    def totalEmployeeSalary: Double = round(
      jobSupport.supportBreakdowns.map(supportBreakdown => supportBreakdown.open.salary).sum
    )

    def totalEmployersGrant: Double = round(
      jobSupport.supportBreakdowns.map(supportBreakdown => supportBreakdown.open.grant).sum
    )

    def totalClosed: Double = round(
      jobSupport.supportBreakdowns.map(supportBreakdown => supportBreakdown.closed.grant).sum
    )

    def totalGrant: Double = round(totalEmployersGrant + totalClosed)

  }

}
