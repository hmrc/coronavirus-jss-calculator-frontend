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

import java.time.Month._
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, Month}

import models.PayFrequency.{FortNightly, FourWeekly, Monthly, Weekly}
import models.{Grant, GrantForPeriod, PayFrequency, PeriodWithHours, SupportClaimPeriod}

import scala.math.BigDecimal.{RoundingMode, double2bigDecimal}

trait RegularPayGrantCalculator {

  def calculateRegularPayGrant(
    periods: List[PeriodWithHours],
    referencePay: BigDecimal,
    supportClaimPeriod: SupportClaimPeriod,
    payFrequency: PayFrequency): Grant = {
    val grantForPeriods: List[GrantForPeriod] = periods.map { period =>
      val grant =
        if (isPartialPeriod(period, supportClaimPeriod))
          calculateGrantForPartialPeriod(referencePay, period, supportClaimPeriod, PayFrequency.payFrequencyDays(payFrequency))
        else {
          calculateGrantForFullPeriod(referencePay, period, payFrequency)
        }
      GrantForPeriod(period, grant)
    }
    Grant(grantForPeriods, isEligibleForGrant(grantForPeriods), grantForPeriods.foldLeft(0.0)((acc, d) => acc + d.amount.doubleValue()))
  }

  def isEligibleForGrant(grantPeriods: List[GrantForPeriod]): Boolean = {
    val totalUsualHours = grantPeriods.foldLeft(0.0)((acc, grantPeriod) => acc + grantPeriod.period.usualHours)
    val totalActualHours = grantPeriods.foldLeft(0.0)((acc, grantPeriod) => acc + grantPeriod.period.actualHours)
    val oneThird = BigDecimal("1") / BigDecimal("3")

    if (BigDecimal(totalActualHours / totalUsualHours).compareTo(oneThird) == 1)
      true
    else
      false
  }

  def calculateGrantForPartialPeriod(
    referencePay: BigDecimal,
    period: PeriodWithHours,
    supportClaimPeriod: SupportClaimPeriod,
    daysInFrequency: Int): Double = {
    val daysInPartialPeriod = calculateEligibleDaysForClaim(period.endDate, supportClaimPeriod, daysInFrequency) + 1
    val claimMonth = supportClaimPeriod.startDate.getMonth
    val referencePayCap = daysInPartialPeriod * RegularPayGrantCalculator.partialPeriodPayCaps.getOrElse(claimMonth, 0.0)
    val adjustedReferencePay = referencePay.doubleValue() * (daysInPartialPeriod.toDouble / daysInFrequency.toDouble)
    val actualReferencePay = scala.math.min(adjustedReferencePay, referencePayCap)
    val grant: BigDecimal =
      ((actualReferencePay * ((period.usualHours - period.actualHours) / period.usualHours)) / 3.0).setScale(2, RoundingMode.HALF_UP)

    grant.doubleValue()
  }

  def calculateGrantForFullPeriod(referencePay: BigDecimal, period: PeriodWithHours, payFrequency: PayFrequency): Double = {
    val referencePayCap = RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)
    val adjustedReferencePay = referencePay.doubleValue()
    val actualReferencePay = scala.math.min(adjustedReferencePay, referencePayCap)
    val grant: BigDecimal =
      ((actualReferencePay * ((period.usualHours - period.actualHours) / period.usualHours)) / 3.0).setScale(2, RoundingMode.HALF_UP)
    grant.doubleValue()
  }

  def calculateEligibleDaysForClaim(periodEndDate: LocalDate, supportClaimPeriod: SupportClaimPeriod, payFrequencyDays: Int): Long =
    scala.math.min(ChronoUnit.DAYS.between(supportClaimPeriod.startDate, periodEndDate), payFrequencyDays)

  def isPartialPeriod(period: PeriodWithHours, supportClaimPeriod: SupportClaimPeriod): Boolean =
    if (period.startDate.isBefore(supportClaimPeriod.startDate)) true else false

}

object RegularPayGrantCalculator {

  val partialPeriodPayCaps: Map[Month, Double] = Map(
    NOVEMBER -> 104.167,
    DECEMBER -> 100.806,
    JANUARY  -> 100.806
  )

  val fullPeriodPayCaps: Map[PayFrequency, Double] = Map(
    Weekly      -> 721.15,
    FortNightly -> 1442.30,
    FourWeekly  -> 2884.60,
    Monthly     -> 3125.00
  )

}
