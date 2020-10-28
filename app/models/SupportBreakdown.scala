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

import java.time.LocalDate

import play.api.libs.json.{Format, Json}

final case class SupportBreakdown(
  startDate: LocalDate,
  endDate: LocalDate,
  daysInPeriod: Int,
  open: OpenJobSupport,
  closed: ClosedJobSupport
)

/*
  //TODO: draft implementation of the calculator - needs refactoring and re-design
 */
object SupportBreakdown {

  implicit val format: Format[SupportBreakdown] = Json.format

  implicit class PeriodSupportOps(private val periodSupports: List[SupportBreakdown]) {
    def totalEmployeeSalary: Double =
      periodSupports.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.salary)

    def totalEmployersGrant: Double = periodSupports.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalClosed: Double = periodSupports.map(s => s.closed).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalGrant: Double = totalEmployersGrant + totalClosed
  }
}
