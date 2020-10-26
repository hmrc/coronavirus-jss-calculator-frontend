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

import services.PeriodSupport

final case class JobSupportOpen(
  salary: Double,
  grant: Double
)

object JobSupportOpen {
  val none = JobSupportOpen(0, 0)
}

final case class JobSupportClosed(
  f: Double
)

final case class JobSupport(
  periodSupport: List[PeriodSupport],
  isEligible: Boolean,
  totalGrant: Double,
  totalSalaryToPayEmployee: Double
)

object JobSupport {

  implicit class JobSupportOps(private val jobSupport: JobSupport) {
    def totalEmployeeSalary: Double =
      jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.salary)

    def totalEmployersGrant = jobSupport.periodSupport.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalClosed = jobSupport.periodSupport.map(s => s.closed).foldLeft(0.0)((acc, f) => acc + f.f)
  }

}
