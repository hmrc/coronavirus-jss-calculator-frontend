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
import java.time.temporal.ChronoUnit

import models.PayFrequency.{FortNightly, FourWeekly, Monthly, Weekly}
import models.{BusinessClosedPeriod, ClosedJobSupport, Interval, JobSupport, OpenJobSupport, PayFrequency, PayPeriod, SupportBreakdown, SupportClaimPeriod, TemporaryWorkingAgreementPeriod}
import utils.MoneyUtils.round

import scala.collection.mutable.ListBuffer

//TODO: put calculations in Try to catch exceptions
//TODO: add logging
//TODO: prevent calculations of closed if no closed period
//TODO: we need a UI constraint which prevents overlapping TWAs or BCs otherwise we will overcompensate for the calculation to compute the number of TWAs or BCs in PP
trait RegularPayGrantCalculator {

  def calculateJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    payPeriods: List[PayPeriod],
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod],
    payFrequency: PayFrequency,
    referencePay: Double
  ): JobSupport = {

    val sortedTWAList = sortedTemporaryWorkingAgreements(temporaryWorkingAgreementPeriods)
    val sortedBCList  = sortedBusinessClosedPeriods(businessClosedPeriods)

    val supportBreakdowns: List[SupportBreakdown] = payPeriods.map { payPeriod =>
      val numberOfTemporaryWorkingAgreementDaysInPayPeriod =
        getTotalNumberOfTemporaryWorkingAgreementDaysInPayPeriod(
          payPeriod,
          getAllTemporaryWorkingAgreementsInThisPayPeriod(payPeriod, sortedTWAList)
        )

      val numberOfBusinessClosedPeriodDaysInPayPeriod = getTotalNumberOfClosedDaysInAPayPeriod(
        payPeriod,
        getAllBusinessClosedPeriodsInThisPayPeriod(payPeriod, sortedBCList)
      )

      val numberOfPayFrequencyDaysInThisPayPeriod = getNumberOfDaysInPayFrequency(payFrequency, payPeriod)

      val usePayFrequencyCap =
        if (
          ((numberOfTemporaryWorkingAgreementDaysInPayPeriod + numberOfBusinessClosedPeriodDaysInPayPeriod)
            >= numberOfPayFrequencyDaysInThisPayPeriod) && (!isPartialPayPeriod(
            payPeriod,
            supportClaimPeriod
          ))
        ) true
        else false

      SupportBreakdown(
        payPeriod.startDate,
        payPeriod.endDate,
        getNumberOfDaysInPayFrequency(payFrequency, payPeriod),
        calculateOpenJobSupport(
          supportClaimPeriod,
          payPeriod,
          getAllTemporaryWorkingAgreementsInThisPayPeriod(payPeriod, sortedTWAList),
          sortedBCList,
          payFrequency,
          referencePay,
          usePayFrequencyCap
        ),
        calculateClosedJobSupport(
          supportClaimPeriod,
          payPeriod,
          getAllBusinessClosedPeriodsInThisPayPeriod(payPeriod, sortedBCList),
          payFrequency,
          referencePay,
          usePayFrequencyCap
        )
      )
    }

    JobSupport(
      supportBreakdowns,
      referencePay
    )
  }

  def calculateOpenJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    payPeriod: PayPeriod,
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod],
    payFrequency: PayFrequency,
    referencePay: Double,
    newCalcFlag: Boolean
  ): OpenJobSupport = {

    val totalNumberOfTemporaryWorkingAgreementDaysInAPayPeriod =
      getTotalNumberOfTemporaryWorkingAgreementDaysInPayPeriod(payPeriod, temporaryWorkingAgreementPeriods)
    val numberOfDaysInPayFrequency                             = getNumberOfDaysInPayFrequency(payFrequency, payPeriod)

    if (
      temporaryWorkingAgreementPeriods.isEmpty || isPayPeriodCompletelyCoveredByBusinessClosedPeriod(
        supportClaimPeriod,
        payPeriod,
        businessClosedPeriods
      ) || isEveryTemporaryWorkingAgreementCompletelyCoveredABusinessClosedPeriod(
        temporaryWorkingAgreementPeriods,
        businessClosedPeriods
      )
    ) {
      OpenJobSupport.zeroFinancialSupport
    } else
      (
        businessClosedPeriods.isEmpty,
        hasOverlappingTemporaryWorkingAgreementPeriodsAndBusinessClosedPeriods(
          temporaryWorkingAgreementPeriods,
          businessClosedPeriods
        )
      ) match {
        case (true, _) =>
          calculateOpenSupport(
            referencePay,
            totalNumberOfTemporaryWorkingAgreementDaysInAPayPeriod,
            numberOfDaysInPayFrequency,
            payFrequency,
            payPeriod,
            newCalcFlag
          )

        case (false, false) =>
          calculateOpenSupport(
            referencePay,
            totalNumberOfTemporaryWorkingAgreementDaysInAPayPeriod,
            numberOfDaysInPayFrequency,
            payFrequency,
            payPeriod,
            newCalcFlag
          )

        case (false, true) =>
          val splicedTemporaryWorkingAgreements =
            spliceTemporaryWorkingAgreementPeriod(temporaryWorkingAgreementPeriods, businessClosedPeriods)

          val totalNumberOfTemporaryWorkingAgreementDaysInAPayPeriod =
            getTotalNumberOfTemporaryWorkingAgreementDaysInPayPeriod(payPeriod, splicedTemporaryWorkingAgreements)

          calculateOpenSupport(
            referencePay,
            totalNumberOfTemporaryWorkingAgreementDaysInAPayPeriod,
            numberOfDaysInPayFrequency,
            payFrequency,
            payPeriod,
            newCalcFlag
          )
      }
  }

  def calculateOpenSupport(
    referencePay: Double,
    totalNumberOfTwaDaysInAPayPeriod: Int,
    numberOfDaysInPayFrequency: Int,
    payFrequency: PayFrequency,
    payPeriod: PayPeriod,
    newCalcFlag: Boolean
  ): OpenJobSupport = {

    val referencePayCap = if (newCalcFlag) {
      scala.math.min(referencePay, newCalculateReferencePayCap(payFrequency))
    } else {
      calculateReferencePayCap(
        totalNumberOfTwaDaysInAPayPeriod,
        !(numberOfDaysInPayFrequency == totalNumberOfTwaDaysInAPayPeriod),
        payFrequency
      )
    }

    val adjustedReferencePay = if (newCalcFlag) {
      proportionReferencePay(referencePayCap, totalNumberOfTwaDaysInAPayPeriod, numberOfDaysInPayFrequency)
    } else {
      proportionReferencePay(referencePay, totalNumberOfTwaDaysInAPayPeriod, numberOfDaysInPayFrequency)
    }

    val actualReferencePay = capReferencePay(adjustedReferencePay, referencePayCap)

    val employeeSalary =
      actualReferencePay * ((payPeriod.usualHours - payPeriod.actualHours) / payPeriod.usualHours) * 0.6667

    val employersGrant = employeeSalary * (61.67 / 66.67)

    OpenJobSupport(
      totalNumberOfTwaDaysInAPayPeriod,
      payPeriod.usualHours,
      payPeriod.actualHours,
      round(employeeSalary),
      round(employersGrant)
    )

  }

  def calculateClosedJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    payPeriod: PayPeriod,
    businessClosedPeriods: List[BusinessClosedPeriod],
    payFrequency: PayFrequency,
    referencePay: Double,
    newCalcFlag: Boolean
  ): ClosedJobSupport = {

    val numberOfClosedDaysInPayPeriod      = getTotalNumberOfClosedDaysInAPayPeriod(payPeriod, businessClosedPeriods)
    val numberOfPayPeriodDaysInClaimPeriod = getNumberOfPayPeriodDaysInClaimDays(payPeriod, supportClaimPeriod)
    val numberOfDaysInPayFrequency         = getNumberOfDaysInPayFrequency(payFrequency, payPeriod)

    val referencePayCap = {
      if (newCalcFlag) {
        scala.math.min(referencePay, newCalculateReferencePayCap(payFrequency))
      } else {
        calculateReferencePayCap(
          numberOfPayPeriodDaysInClaimPeriod,
          !(numberOfDaysInPayFrequency == numberOfClosedDaysInPayPeriod),
          payFrequency
        )
      }
    }

    val adjustedReferencePay =
      calculateAdjustedReferencePay(referencePay, numberOfDaysInPayFrequency, numberOfPayPeriodDaysInClaimPeriod)

    val actualReferencePay = capReferencePay(adjustedReferencePay, referencePayCap)

    val closedSupportGrant = round(
      actualReferencePay * (numberOfClosedDaysInPayPeriod.toDouble / numberOfPayPeriodDaysInClaimPeriod.toDouble) * 0.6667
    )

    ClosedJobSupport(numberOfClosedDaysInPayPeriod, closedSupportGrant)

  }

  def isPayPeriodWithinClaimPeriod(payPeriod: PayPeriod, supportClaimPeriod: SupportClaimPeriod): Boolean =
    if (
      (
        (payPeriod.startDate
          .isEqual(supportClaimPeriod.startDate) && payPeriod.endDate.isBefore(supportClaimPeriod.endDate))
        ||
        (payPeriod.startDate
          .isEqual(supportClaimPeriod.startDate) && payPeriod.endDate.isEqual(supportClaimPeriod.endDate))
        ||
        (payPeriod.startDate
          .isAfter(supportClaimPeriod.startDate) && payPeriod.endDate.isEqual(supportClaimPeriod.endDate))
        ||
        (payPeriod.startDate
          .isAfter(supportClaimPeriod.startDate) && payPeriod.endDate.isBefore(supportClaimPeriod.startDate))
      )
    )
      true
    else
      false

  def isPayPeriodCoveredCompletelyByEitherATemporaryWorkingAgreementAndOrBusinessClosedPeriod(
    payPeriod: PayPeriod,
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Boolean = {
    val daysInPayPeriod = ChronoUnit.DAYS.between(payPeriod.startDate, payPeriod.endDate) + 1
    (temporaryWorkingAgreementPeriods.isEmpty, businessClosedPeriods.isEmpty) match {
      case (true, true)   => false
      case (false, true)  =>
        val totalNumberOfBusinessClosedDaysCoveringThisPayPeriod = businessClosedPeriods
          .map(closedPeriod => ChronoUnit.DAYS.between(closedPeriod.startDate, closedPeriod.endDate) + 1)
          .sum
        if (totalNumberOfBusinessClosedDaysCoveringThisPayPeriod < daysInPayPeriod) false else true
      case (true, false)  =>
        val totalNumberOfTemporaryWorkingAgreementsInThisPeriod =
          temporaryWorkingAgreementPeriods
            .map(temporaryWorkingAgreementPeriod =>
              ChronoUnit.DAYS
                .between(temporaryWorkingAgreementPeriod.startDate, temporaryWorkingAgreementPeriod.endDate) + 1
            )
            .sum
        if (totalNumberOfTemporaryWorkingAgreementsInThisPeriod < daysInPayPeriod) false else true
      case (false, false) =>
        // This is the case where there can be overlaps. However the overlaps can happen in many different ways
        // Best to consider the fewer? cases where they do overlap
        val trimmedTemporaryWorkingAgreementsToPayPeriod = temporaryWorkingAgreementPeriods
          .map(twa => trimTemporaryWorkingAgreementToPayPeriod(payPeriod, twa))
          .map(s => Interval(s.startDate, s.endDate))

        val trimmedBusinessClosedPeriodsToPayPeriod = sortedBusinessClosedPeriods(
          getAllBusinessClosedPeriodsInThisPayPeriod(payPeriod, businessClosedPeriods)
        ).map(bc => trimBusinessClosedPeriodToPayPeriod(payPeriod, bc)).map(s => Interval(s.startDate, s.endDate))

        val sortedIntervals = sortIntervals(
          trimmedTemporaryWorkingAgreementsToPayPeriod ::: trimmedBusinessClosedPeriodsToPayPeriod
        )

        sortedIntervals.sliding(2).exists(intervals => hasIntervalGap(intervals))
    }
  }

  def hasIntervalGap(dates: List[Interval]): Boolean = {
    val firstInterval  = dates.head
    val secondInterval = dates.last

    if (ChronoUnit.DAYS.between(firstInterval.endDate, secondInterval.startDate) > 1) {
      true
    } else
      false
  }

  def trimBusinessClosedPeriodToPayPeriod(
    payPeriod: PayPeriod,
    businessClosedPeriod: BusinessClosedPeriod
  ): BusinessClosedPeriod =
    if (
      businessClosedPeriod.startDate.isBefore(payPeriod.startDate) && businessClosedPeriod.endDate
        .isEqual(payPeriod.startDate)
    ) {
      businessClosedPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      businessClosedPeriod.startDate.isEqual(payPeriod.endDate) && businessClosedPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      businessClosedPeriod.copy(endDate = payPeriod.endDate)
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && (businessClosedPeriod.endDate.isAfter(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate))
    ) {
      businessClosedPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      businessClosedPeriod.startDate
        .isBefore(payPeriod.endDate) && (businessClosedPeriod.endDate.isAfter(payPeriod.endDate))
    ) {
      businessClosedPeriod.copy(endDate = payPeriod.endDate)
    } else if (
      businessClosedPeriod.startDate.isEqual(payPeriod.startDate) && businessClosedPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      businessClosedPeriod.copy(endDate = payPeriod.endDate)
    } else if (
      businessClosedPeriod.startDate.isBefore(payPeriod.startDate) && businessClosedPeriod.endDate
        .isEqual(payPeriod.endDate)
    ) {
      businessClosedPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      businessClosedPeriod.startDate.isBefore(payPeriod.startDate) && businessClosedPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      businessClosedPeriod.copy(startDate = payPeriod.startDate, endDate = payPeriod.endDate)
    } else businessClosedPeriod

  def trimTemporaryWorkingAgreementToPayPeriod(
    payPeriod: PayPeriod,
    temporaryWorkingAgreementPeriod: TemporaryWorkingAgreementPeriod
  ): TemporaryWorkingAgreementPeriod =
    if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(payPeriod.startDate) && temporaryWorkingAgreementPeriod.endDate
        .isEqual(payPeriod.startDate)
    ) {
      temporaryWorkingAgreementPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(payPeriod.endDate) && temporaryWorkingAgreementPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      temporaryWorkingAgreementPeriod.copy(endDate = payPeriod.endDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && (temporaryWorkingAgreementPeriod.endDate.isAfter(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isBefore(payPeriod.endDate))
    ) {
      temporaryWorkingAgreementPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate
        .isBefore(payPeriod.endDate) && (temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.endDate))
    ) {
      temporaryWorkingAgreementPeriod.copy(startDate = payPeriod.startDate, endDate = payPeriod.endDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(payPeriod.startDate) && temporaryWorkingAgreementPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      temporaryWorkingAgreementPeriod.copy(endDate = payPeriod.endDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(payPeriod.startDate) && temporaryWorkingAgreementPeriod.endDate
        .isEqual(payPeriod.endDate)
    ) {
      temporaryWorkingAgreementPeriod.copy(startDate = payPeriod.startDate)
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(payPeriod.startDate) && temporaryWorkingAgreementPeriod.endDate
        .isAfter(payPeriod.endDate)
    ) {
      temporaryWorkingAgreementPeriod.copy(startDate = payPeriod.startDate, endDate = payPeriod.endDate)
    } else {
      temporaryWorkingAgreementPeriod
    }

  def getTotalNumberOfClosedDaysInAPayPeriod(
    payPeriod: PayPeriod,
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Int = businessClosedPeriods.map(bc => getNumberOfClosedDaysInAPayPeriod(payPeriod, bc)).sum

  def getNumberOfClosedDaysInAPayPeriod(
    payPeriod: PayPeriod,
    businessClosedPeriod: BusinessClosedPeriod
  ): Int =
    if (
      (businessClosedPeriod.startDate.isEqual(
        businessClosedPeriod.endDate
      ) && businessClosedPeriod.startDate.isEqual(payPeriod.startDate))
      ||
      businessClosedPeriod.endDate.isEqual(payPeriod.startDate)
      ||
      businessClosedPeriod.startDate.isEqual(payPeriod.endDate)
    ) 1
    else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
      ||
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
      ||
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
      ||
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS.between(payPeriod.startDate, payPeriod.endDate).toInt + 1
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS.between(payPeriod.startDate, businessClosedPeriod.endDate).toInt + 1
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS.between(payPeriod.startDate, businessClosedPeriod.endDate).toInt + 1
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS
        .between(businessClosedPeriod.startDate, businessClosedPeriod.endDate)
        .toInt + 1
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS
        .between(businessClosedPeriod.startDate, businessClosedPeriod.endDate)
        .toInt + 1
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      ChronoUnit.DAYS.between(businessClosedPeriod.startDate, payPeriod.endDate).toInt + 1
    } else
      0

  def isPayPeriodCompletelyCoveredByBusinessClosedPeriod(
    supportClaimPeriod: SupportClaimPeriod,
    payPeriod: PayPeriod,
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Boolean =
    if (isPartialPayPeriod(payPeriod, supportClaimPeriod)) {
      val adjustedPayPeriod = chopPartialPayPeriod(payPeriod, supportClaimPeriod)
      businessClosedPeriods.exists(businessClosedPeriod =>
        (businessClosedPeriod.startDate.isEqual(adjustedPayPeriod.startDate) && businessClosedPeriod.endDate
          .isEqual(adjustedPayPeriod.endDate))
          ||
            (businessClosedPeriod.startDate.isEqual(
              adjustedPayPeriod.startDate
            ) && businessClosedPeriod.endDate.isAfter(adjustedPayPeriod.endDate))
            ||
            (businessClosedPeriod.startDate.isBefore(
              adjustedPayPeriod.startDate
            ) && businessClosedPeriod.endDate.isEqual(adjustedPayPeriod.endDate))
            ||
            (businessClosedPeriod.startDate.isBefore(
              adjustedPayPeriod.startDate
            ) && businessClosedPeriod.endDate.isAfter(adjustedPayPeriod.endDate))
      )

    } else {
      businessClosedPeriods.exists(businessClosedPeriod =>
        (businessClosedPeriod.startDate.isEqual(payPeriod.startDate) && businessClosedPeriod.endDate
          .isEqual(payPeriod.endDate))
          ||
            (businessClosedPeriod.startDate.isEqual(
              payPeriod.startDate
            ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate))
            ||
            (businessClosedPeriod.startDate.isBefore(
              payPeriod.startDate
            ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate))
            ||
            (businessClosedPeriod.startDate.isBefore(
              payPeriod.startDate
            ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate))
      )
    }

  def chopPartialPayPeriod(payPeriod: PayPeriod, supportClaimPeriod: SupportClaimPeriod): PayPeriod =
    if (
      (payPeriod.startDate.isBefore(supportClaimPeriod.startDate) && (payPeriod.endDate.isBefore(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
    ) {
      payPeriod.copy(startDate = supportClaimPeriod.startDate)
    } else if (
      (payPeriod.startDate.isBefore(supportClaimPeriod.startDate) && (payPeriod.endDate.isBefore(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isEqual(supportClaimPeriod.startDate)))
    ) {
      payPeriod.copy(startDate = supportClaimPeriod.startDate, endDate = supportClaimPeriod.startDate)
    } else if (
      (payPeriod.startDate.isBefore(supportClaimPeriod.endDate) && (payPeriod.endDate.isAfter(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
    ) {
      payPeriod.copy(endDate = supportClaimPeriod.endDate)
    } else if (
      (payPeriod.startDate.isEqual(supportClaimPeriod.startDate) && (payPeriod.endDate.isAfter(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
    ) {
      payPeriod.copy(startDate = supportClaimPeriod.endDate, endDate = supportClaimPeriod.endDate)
    } else {
      payPeriod
    }

  def isPartialPayPeriod(payPeriod: PayPeriod, supportClaimPeriod: SupportClaimPeriod): Boolean =
    if (
      (payPeriod.startDate.isBefore(supportClaimPeriod.startDate) && (payPeriod.endDate.isBefore(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
      ||
      (payPeriod.startDate.isBefore(supportClaimPeriod.startDate) && (payPeriod.endDate.isBefore(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isEqual(supportClaimPeriod.startDate)))
      ||
      (payPeriod.startDate.isBefore(supportClaimPeriod.endDate) && (payPeriod.endDate.isAfter(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
      ||
      (payPeriod.startDate.isEqual(supportClaimPeriod.startDate) && (payPeriod.endDate.isAfter(
        supportClaimPeriod.endDate
      ) && payPeriod.endDate.isAfter(supportClaimPeriod.startDate)))
    ) true
    else false

  def isTemporaryWorkingAgreementCompletelyCoveredByABusinessClosedPeriod(
    temporaryWorkingAgreementPeriod: TemporaryWorkingAgreementPeriod,
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Boolean =
    businessClosedPeriods.exists(businessClosedPeriod =>
      (businessClosedPeriod.startDate.isEqual(temporaryWorkingAgreementPeriod.startDate) && businessClosedPeriod.endDate
        .isEqual(temporaryWorkingAgreementPeriod.endDate))
        ||
          (businessClosedPeriod.startDate.isEqual(
            temporaryWorkingAgreementPeriod.startDate
          ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate))
          ||
          (businessClosedPeriod.startDate.isBefore(
            temporaryWorkingAgreementPeriod.startDate
          ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.endDate))
          ||
          (businessClosedPeriod.startDate.isBefore(
            temporaryWorkingAgreementPeriod.startDate
          ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate))
    )

  def isEveryTemporaryWorkingAgreementCompletelyCoveredABusinessClosedPeriod(
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Boolean =
    temporaryWorkingAgreementPeriods.forall(temporaryWorkingAgreementPeriod =>
      isTemporaryWorkingAgreementCompletelyCoveredByABusinessClosedPeriod(
        temporaryWorkingAgreementPeriod,
        businessClosedPeriods
      )
    )

  def spliceTemporaryWorkingAgreementPeriod(
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): List[TemporaryWorkingAgreementPeriod] = {
    val splicedTemporaryWorkingAgreementSegments = ListBuffer[LocalDate]()
    for (temporaryWorkingAgreementPeriod <- temporaryWorkingAgreementPeriods) {
      splicedTemporaryWorkingAgreementSegments += temporaryWorkingAgreementPeriod.startDate
      // now get all the closed period that are in this TWA and pass that into the next for loop
      val businessClosedPeriodsWhichOverlapTemporaryWorkingAgreementPeriod = sortedBusinessClosedPeriods(
        getAllBusinessClosedPeriodsWhichOverlapTemporaryWorkingAgreementPeriod(
          temporaryWorkingAgreementPeriod,
          businessClosedPeriods
        )
      )
      for (businessClosedPeriod <- businessClosedPeriodsWhichOverlapTemporaryWorkingAgreementPeriod) {
        splicedTemporaryWorkingAgreementSegments += businessClosedPeriod.startDate.minusDays(1)
        splicedTemporaryWorkingAgreementSegments += businessClosedPeriod.endDate.plusDays(1)
      }
      splicedTemporaryWorkingAgreementSegments += temporaryWorkingAgreementPeriod.endDate
    }
    // now we have all the date points to construct a list of Temporary Working Agreement Periods which represent the open porition of the pay period
    splicedTemporaryWorkingAgreementSegments.toList
      .grouped(2)
      .map(localDates => TemporaryWorkingAgreementPeriod(localDates.head, localDates.last))
      .toList
  }

  def getAllBusinessClosedPeriodsInThisPayPeriod(
    payPeriod: PayPeriod,
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): List[BusinessClosedPeriod] =
    businessClosedPeriods.filter(businessClosedPeriod =>
      isBusinessClosedPeriodInPayPeriod(payPeriod, businessClosedPeriod)
    )

  //TODO: check if this is not eliminating valid TWAs
  // this will get all the TWAs inside this PP but also will only return that portion of the TWA inside the PP, otherwise the splicing function won't work as expect as the dates for the TWA will be outside the PP
  def getAllTemporaryWorkingAgreementsInThisPayPeriod(
    payPeriod: PayPeriod,
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod]
  ): List[TemporaryWorkingAgreementPeriod] = temporaryWorkingAgreementPeriods.filter(temporaryWorkingAgreementPeriod =>
    isTemporaryWorkingAgreementPeriodInPayPeriod(payPeriod, temporaryWorkingAgreementPeriod)
  )

  def getAllBusinessClosedPeriodsWhichOverlapTemporaryWorkingAgreementPeriod(
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementPeriod,
    businessClosedWithDates: List[BusinessClosedPeriod]
  ): List[BusinessClosedPeriod] =
    businessClosedWithDates.filter(businessClosedPeriod =>
      isBusinessClosedPeriodInTemporaryWorkingAgreementPeriod(temporaryWorkingAgreementWithDates, businessClosedPeriod)
    )

  def isBusinessClosedPeriodInPayPeriod(
    payPeriod: PayPeriod,
    businessClosedPeriod: BusinessClosedPeriod
  ): Boolean =
    if (
      businessClosedPeriod.startDate.isEqual(
        businessClosedPeriod.endDate
      ) && businessClosedPeriod.startDate.isEqual(payPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && (businessClosedPeriod.startDate.isBefore(payPeriod.endDate) | businessClosedPeriod.startDate
        .isEqual(payPeriod.endDate))
      && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && (businessClosedPeriod.startDate.isBefore(payPeriod.endDate) | businessClosedPeriod.startDate
        .isEqual(payPeriod.endDate))
      && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.endDate
      ) && (businessClosedPeriod.startDate.isBefore(payPeriod.endDate) | businessClosedPeriod.startDate
        .isEqual(payPeriod.endDate))
      && businessClosedPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        payPeriod.endDate
      ) && businessClosedPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else {
      false
    }

  def isBusinessClosedPeriodInTemporaryWorkingAgreementPeriod(
    temporaryWorkingAgreementPeriod: TemporaryWorkingAgreementPeriod,
    businessClosedPeriod: BusinessClosedPeriod
  ): Boolean =
    if (
      businessClosedPeriod.startDate.isEqual(
        businessClosedPeriod.endDate
      ) && businessClosedPeriod.startDate.isEqual(temporaryWorkingAgreementPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.startDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isBefore(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isBefore(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isAfter(
        temporaryWorkingAgreementPeriod.startDate
      ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.endDate
      ) && businessClosedPeriod.endDate.isEqual(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else if (
      businessClosedPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.endDate
      ) && businessClosedPeriod.endDate.isAfter(temporaryWorkingAgreementPeriod.endDate)
    ) {
      true
    } else {
      false
    }

  def isTemporaryWorkingAgreementPeriodInPayPeriod(
    payPeriod: PayPeriod,
    temporaryWorkingAgreementPeriod: TemporaryWorkingAgreementPeriod
  ): Boolean =
    if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        temporaryWorkingAgreementPeriod.endDate
      ) && temporaryWorkingAgreementPeriod.startDate.isEqual(payPeriod.startDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isEqual(payPeriod.startDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.startDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.startDate
      ) && temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      (temporaryWorkingAgreementPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && (temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.endDate
      ) || temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.endDate
      )))

      && temporaryWorkingAgreementPeriod.endDate.isBefore(payPeriod.endDate)
    ) {
      true
    } else if (
      (temporaryWorkingAgreementPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && (temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.endDate
      ) || temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.endDate
      )))

      && temporaryWorkingAgreementPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      (temporaryWorkingAgreementPeriod.startDate.isAfter(
        payPeriod.startDate
      ) && (temporaryWorkingAgreementPeriod.startDate.isBefore(
        payPeriod.endDate
      ) || temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.endDate
      )))

      && temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.endDate
      ) && temporaryWorkingAgreementPeriod.endDate.isEqual(payPeriod.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementPeriod.startDate.isEqual(
        payPeriod.endDate
      ) && temporaryWorkingAgreementPeriod.endDate.isAfter(payPeriod.endDate)
    ) {
      true
    } else {
      false
    }

  def calculateNumberOfTemporaryWorkingAgreementDaysInPayPeriod(
    periodWithHours: PayPeriod,
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementPeriod
  ): Int =
    if (
      (temporaryWorkingAgreementWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && temporaryWorkingAgreementWithDates.startDate.isEqual(periodWithHours.startDate))
      ||
      temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.startDate)
      ||
      temporaryWorkingAgreementWithDates.startDate.isEqual(periodWithHours.endDate)
    )
      1
    else if (
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
      ||
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
      ||
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
      ||
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) ChronoUnit.DAYS.between(periodWithHours.startDate, periodWithHours.endDate).toInt + 1
    else if (
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      ChronoUnit.DAYS.between(periodWithHours.startDate, temporaryWorkingAgreementWithDates.endDate).toInt + 1
    } else if (
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      ChronoUnit.DAYS.between(periodWithHours.startDate, temporaryWorkingAgreementWithDates.endDate).toInt + 1
    } else if (
      temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      ChronoUnit.DAYS
        .between(temporaryWorkingAgreementWithDates.startDate, temporaryWorkingAgreementWithDates.endDate)
        .toInt + 1
    } else if (
      temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      ChronoUnit.DAYS
        .between(temporaryWorkingAgreementWithDates.startDate, temporaryWorkingAgreementWithDates.endDate)
        .toInt + 1

    } else if (
      temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      ChronoUnit.DAYS.between(temporaryWorkingAgreementWithDates.startDate, periodWithHours.endDate).toInt + 1
    } else
      0

  def getTotalNumberOfTemporaryWorkingAgreementDaysInPayPeriod(
    periodWithHours: PayPeriod,
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementPeriod]
  ): Int = {
    val temporaryWorkingAgreementPeriodsInThisPayPeriod =
      temporaryWorkingAgreementWithDates.filter(temporaryWorkingAgreementPeriod =>
        isTemporaryWorkingAgreementPeriodInPayPeriod(periodWithHours, temporaryWorkingAgreementPeriod)
      )
    temporaryWorkingAgreementPeriodsInThisPayPeriod
      .map(temporaryWorkingAgreementPeriod =>
        calculateNumberOfTemporaryWorkingAgreementDaysInPayPeriod(periodWithHours, temporaryWorkingAgreementPeriod)
      )
      .sum
  }

  def sortedTemporaryWorkingAgreements(
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod]
  ): List[TemporaryWorkingAgreementPeriod] =
    temporaryWorkingAgreementPeriods.sortWith((x, y) => x.endDate.isBefore(y.endDate))

  def sortedBusinessClosedPeriods(businessClosedPeriods: List[BusinessClosedPeriod]): List[BusinessClosedPeriod] =
    businessClosedPeriods.sortWith((x, y) => x.endDate.isBefore(y.endDate))

  def sortIntervals(intervals: List[Interval]): List[Interval] =
    intervals.sortWith((x, y) => x.endDate.isBefore(y.startDate))

  private def isTemporaryWorkingAgreementPeriodOverlappedByBusinessClosedPeriod(
    temporaryWorkingAgreementPeriod: TemporaryWorkingAgreementPeriod,
    businessClosedPeriod: BusinessClosedPeriod
  ): Boolean =
    if (
      temporaryWorkingAgreementPeriod.startDate.isAfter(
        businessClosedPeriod.endDate
      ) || businessClosedPeriod.startDate.isAfter(temporaryWorkingAgreementPeriod.endDate)
    ) false
    else true

  def hasOverlappingTemporaryWorkingAgreementPeriodsAndBusinessClosedPeriods(
    temporaryWorkingAgreementPeriods: List[TemporaryWorkingAgreementPeriod],
    businessClosedPeriods: List[BusinessClosedPeriod]
  ): Boolean =
    sortedTemporaryWorkingAgreements(temporaryWorkingAgreementPeriods).exists(temporaryWorkingAgreementPeriod =>
      sortedBusinessClosedPeriods(businessClosedPeriods).exists(businessClosedPeriod =>
        isTemporaryWorkingAgreementPeriodOverlappedByBusinessClosedPeriod(
          temporaryWorkingAgreementPeriod,
          businessClosedPeriod
        )
      )
    )

  def getNumberOfDaysInPayFrequency(payFrequency: PayFrequency, periodWithHours: PayPeriod): Int =
    payFrequency match {
      case PayFrequency.Weekly      => PayFrequency.payFrequencyDays(Weekly)
      case PayFrequency.FortNightly => PayFrequency.payFrequencyDays(FortNightly)
      case PayFrequency.FourWeekly  => PayFrequency.payFrequencyDays(FourWeekly)
      case PayFrequency.Monthly     => calculateFrequencyDaysForMonthlyFrequency(periodWithHours)
    }

  /*
    I_support-claim-period  = [a_s, a_e]
    I_period                = [b_s, b_e]
    if ( b_s > a_e or as>be ) { return 0 }
      else {
        o_s = max(a_s, b_s)
        o_e = min(a_e, b_e)
        return [o_s,o_e]
    }
   */
  def getNumberOfPayPeriodDaysInClaimDays(
    periodWithHours: PayPeriod,
    supportClaimPeriod: SupportClaimPeriod
  ): Int =
    if (
      supportClaimPeriod.startDate
        .isAfter(periodWithHours.endDate) || periodWithHours.startDate.isAfter(supportClaimPeriod.endDate)
    ) 0
    else {
      val start = scala.math.max(supportClaimPeriod.startDate.toEpochDay, periodWithHours.startDate.toEpochDay)
      val end   = scala.math.min(supportClaimPeriod.endDate.toEpochDay, periodWithHours.endDate.toEpochDay)
      ChronoUnit.DAYS.between(LocalDate.ofEpochDay(start), LocalDate.ofEpochDay(end)).toInt + 1
    }

  private def calculateAdjustedReferencePay(
    referencePay: Double,
    daysInPeriod: Int,
    qualifyingDaysInPeriod: Int
  ): Double =
    (referencePay / daysInPeriod.toDouble) * qualifyingDaysInPeriod.toDouble

  private def proportionReferencePay(referencePay: Double, daysInPeriod: Int, qualifyingDaysInPeriod: Int): Double =
    referencePay * (daysInPeriod.toDouble / qualifyingDaysInPeriod.toDouble)

  private def calculateReferencePayCap(
    daysInPeriod: Int,
    isPartialPeriod: Boolean,
    payFrequency: PayFrequency
  ): Double =
    if (isPartialPeriod) daysInPeriod * 102.74
    else RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)

  private def newCalculateReferencePayCap(
    payFrequency: PayFrequency
  ): Double =
    RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)

  private def capReferencePay(referencePay: Double, referencePayCap: Double): Double =
    scala.math.min(referencePay, referencePayCap)

  private def calculateFrequencyDaysForMonthlyFrequency(periodWithHours: PayPeriod): Int =
    ChronoUnit.DAYS.between(periodWithHours.startDate, periodWithHours.endDate).toInt + 1
}

object RegularPayGrantCalculator {

  val fullPeriodPayCaps: Map[PayFrequency, Double] = Map(
    Weekly      -> 721.15,
    FortNightly -> 1442.30,
    FourWeekly  -> 2884.60,
    Monthly     -> 3125.00
  )

}
