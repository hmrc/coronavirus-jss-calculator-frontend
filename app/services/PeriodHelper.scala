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

package services

import java.time.LocalDate
import java.time.temporal.TemporalAdjusters.lastDayOfMonth

import models.{PayFrequency, Period, SupportClaimPeriod}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait PeriodHelper {

  def getPayPeriods(lastPayDay: LocalDate, payFrequency: PayFrequency, supportClaimPeriod: SupportClaimPeriod): List[Period] =
    payFrequency match {
      case PayFrequency.Monthly =>
        if (lastPayDay.isEqual(lastPayDay.`with`(lastDayOfMonth()))) {
          List(Period(lastPayDay, supportClaimPeriod.endDate))
        } else {
          List(Period(lastPayDay, lastPayDay.plusMonths(1).minusDays(1)))
        }
      case _ =>
        computePayPeriods(PayFrequency.paymentFrequencyDays(payFrequency), lastPayDay, supportClaimPeriod)
    }

  def computePayPeriods(freqDays: Int, lastPayDay: LocalDate, jobSupportClaimPeriod: SupportClaimPeriod): List[Period] = {
    var sp = lastPayDay
    val e: mutable.ListBuffer[Period] = ListBuffer()
    while (!sp.plusDays(freqDays - 1).isAfter(jobSupportClaimPeriod.endDate)) {
      e += Period(sp, sp.plusDays(freqDays - 1))
      sp = sp.plusDays(freqDays)
    }
    sortedEndDates(e.toList)
  }

  private def sortedEndDates(in: List[Period]): List[Period] = in.sortWith((x, y) => x.endDate.isBefore(y.endDate))

}
