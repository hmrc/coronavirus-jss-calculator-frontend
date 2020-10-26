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

final case class JobSupportOpen(
  twaDays: Int,
  usualHours: Double,
  actualHours: Double,
  salary: Double,
  grant: Double
)

object JobSupportOpen {
  val noSupport = JobSupportOpen(0, 0, 0, 0, 0)
}

final case class JobSupportClosed(
  closedDays: Int,
  grant: Double
)

object JobSupportClosed {
  val noSupport = JobSupportClosed(0, 0)
}

final case class JobSupport(
  periodSupport: List[PeriodSupport],
  referenceSalary: Double
)

object JobSupport {

  implicit class JobSupportOps(private val jobSupport: JobSupport) {

    def totalActualHours = jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.actualHours)

    def totalUsualHours = jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.usualHours)

    def isEligible: Boolean = (totalUsualHours / totalActualHours) > 0.20

    def totalEmployeeSalary: Double =
      jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.salary)

    def totalEmployersGrant: Double = jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalClosed: Double =
      jobSupport.periodSupport.map(support => support.closed).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalGrant: Double = totalEmployersGrant + totalClosed
  }

}
