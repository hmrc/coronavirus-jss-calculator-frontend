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
import models.{BusinessClosedWithDates, ClosedJobSupport, JobSupport, OpenJobSupport, PayFrequency, PeriodWithHours, SupportBreakdown, SupportClaimPeriod, TemporaryWorkingAgreementWithDates}

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode

trait RegularPayGrantCalculator {

  def calculateJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    payPeriods: List[PeriodWithHours],
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates],
    businessClosedWithDates: List[BusinessClosedWithDates],
    payFrequency: PayFrequency,
    referencePay: Double
  ): JobSupport = {

    //TODO: refactor: general sorting function
    val sortedTWAList = sortedTWA(temporaryWorkingAgreementWithDates)
    val sortedBCList  = sortedBusinessClosed(businessClosedWithDates)

    val results: List[SupportBreakdown] = payPeriods.map { payPeriod =>
      //TODO: refactor: assess whether this pay period needs to run both calculation (efficiency)
      SupportBreakdown(
        payPeriod.startDate,
        payPeriod.endDate,
        getNumberOfDaysInPayFrequency(payFrequency, payPeriod),
        calculateOpenJobSupport(
          supportClaimPeriod,
          payPeriod,
          getAllTwasInThisPayPeriod(payPeriod, sortedTWAList, sortedBCList),
          sortedBCList,
          payFrequency,
          referencePay
        ),
        calculateClosedJobSupport(
          supportClaimPeriod,
          payPeriod,
          getAllBcsInThisPayPeriod(payPeriod, sortedBCList),
          payFrequency,
          referencePay
        )
      )
    }

    JobSupport(
      results,
      referencePay
    )
  }

  def calculateOpenJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates],
    businessClosedWithDates: List[BusinessClosedWithDates],
    payFrequency: PayFrequency,
    referencePay: Double
  ): OpenJobSupport =
    if (temporaryWorkingAgreementWithDates.isEmpty) {
      OpenJobSupport.noSupport
    } else if (ppCoveredByBC(periodWithHours, businessClosedWithDates)) {
      OpenJobSupport.noSupport
    } else
      (
        hasOnlyTwaPeriods(businessClosedWithDates),
        hasOverlappingTwaAndBusinessClosedPeriods(temporaryWorkingAgreementWithDates, businessClosedWithDates)
      ) match {
        case (true, _) =>
          val numberOfDaysInTwa = totalNumberOfTwaDaysInPayPeriod(periodWithHours, temporaryWorkingAgreementWithDates)

          // THis is the number of days a pay period is in the claim period
          val _: Int = getNumberOfPayPeriodDaysInClaimDays(periodWithHours, supportClaimPeriod)

          // This returns the number of days in the frequency
          val freqDays = getNumberOfDaysInPayFrequency(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, numberOfDaysInTwa, freqDays)

          // This gets the cap to be applied and it is apportioned appropriately
          val step4 = calculateReferencePayCap(numberOfDaysInTwa, !(freqDays == numberOfDaysInTwa), payFrequency)

          // Apply the pay cap
          val step5 = capReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          OpenJobSupport(
            numberOfDaysInTwa,
            periodWithHours.usualHours,
            periodWithHours.actualHours,
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )

        case (false, false) =>
          val s = totalNumberOfTwaDaysInPayPeriod(periodWithHours, temporaryWorkingAgreementWithDates)

          // This returns the number of days in the frequency
          val freqDays = getNumberOfDaysInPayFrequency(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, s, freqDays)

          // This gets the cap to be applied and it is approptioned appropriately
          val step4 = calculateReferencePayCap(s, !(freqDays == s), payFrequency)

          // Appyly the pay cap
          val step5 = capReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          OpenJobSupport(
            s,
            periodWithHours.usualHours,
            periodWithHours.actualHours,
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )

        case (false, true) =>
          val adjustedTwaList =
            getAllTwasInThisPayPeriod(periodWithHours, temporaryWorkingAgreementWithDates, businessClosedWithDates)

          val newAdjustedTwas = twaSurgery(adjustedTwaList, businessClosedWithDates)

          val s = totalNumberOfTwaDaysInPayPeriod(periodWithHours, newAdjustedTwas)

          // This returns the number of days in the frequency
          val freqDays = getNumberOfDaysInPayFrequency(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, s, freqDays)

          // This gets the cap to be applied and it is approptioned appropriately
          val step4 = calculateReferencePayCap(s, !(freqDays == s), payFrequency)

          // Appyly the pay cap
          val step5 = capReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          OpenJobSupport(
            s,
            periodWithHours.usualHours,
            periodWithHours.actualHours,
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )
      }

  def calculateClosedJobSupport(
    supportClaimPeriod: SupportClaimPeriod,
    periodWithHours: PeriodWithHours,
    businessClosedDates: List[BusinessClosedWithDates],
    payFrequency: PayFrequency,
    referencePay: Double
  ): ClosedJobSupport = {

    val numberOfClosedDaysInPayPeriod      = getNumberOfClosedDaysInAPayPeriod(periodWithHours, businessClosedDates)
    val numberOfPayPeriodDaysInClaimPeriod = getNumberOfPayPeriodDaysInClaimDays(periodWithHours, supportClaimPeriod)
    val numberOfDaysInPayFrequency         = getNumberOfDaysInPayFrequency(payFrequency, periodWithHours)
    val adjustedReferencePay               =
      calculateAdjustedReferencePay(referencePay, numberOfDaysInPayFrequency, numberOfPayPeriodDaysInClaimPeriod)

    val referencePayCap = calculateReferencePayCap(
      numberOfPayPeriodDaysInClaimPeriod,
      !(numberOfDaysInPayFrequency == numberOfClosedDaysInPayPeriod),
      payFrequency
    )

    val cap = capReferencePay(adjustedReferencePay, referencePayCap)

    val closedSupportGrant =
      BigDecimal(
        cap * (numberOfClosedDaysInPayPeriod.toDouble / numberOfPayPeriodDaysInClaimPeriod.toDouble) * 2.0 / 3.0
      ).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

    ClosedJobSupport(numberOfClosedDaysInPayPeriod, closedSupportGrant)

  }

  def getNumberOfClosedDaysInAPayPeriod(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): Int =
    businessClosedWithDates.foldLeft(0)((acc, bc) => acc + getNumberOfClosedDaysInPayPeriod(periodWithHours, bc))

  def getNumberOfClosedDaysInPayPeriod(
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: BusinessClosedWithDates
  ): Int =
    if (
      (temporaryWorkingAgreementWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && temporaryWorkingAgreementWithDates.startDate.isEqual(periodWithHours.startDate))
      ||
      temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.startDate)
      ||
      temporaryWorkingAgreementWithDates.startDate.isEqual(periodWithHours.endDate)
    ) 1
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

  def removeAllPeriodsCoveredCompletelyByClosedPeriods(
    periodWithHours: List[PeriodWithHours],
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): List[PeriodWithHours] =
    periodWithHours.filterNot(twa => isPPCovered(twa, businessClosedWithDates))

  def isPPCovered(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): Boolean =
    businessClosedWithDates.exists(bc =>
      (periodWithHours.startDate.isEqual(bc.startDate) && periodWithHours.endDate
        .isEqual(bc.endDate))
        ||
          (
            (periodWithHours.startDate
              .isBefore(bc.startDate) && periodWithHours.endDate
              .isBefore(bc.endDate))
          )
    )

  def ppCoveredByBC(periodWithHours: PeriodWithHours, businessClosedWithDates: List[BusinessClosedWithDates]): Boolean =
    businessClosedWithDates.exists(bc =>
      bc.startDate.isBefore(periodWithHours.startDate) && bc.endDate.isAfter(periodWithHours.endDate) || bc.startDate
        .isEqual(periodWithHours.startDate) && bc.endDate.isEqual(periodWithHours.endDate)
    )

  def twaSurgery(
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates],
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): List[TemporaryWorkingAgreementWithDates] = {

    val datePoints = ListBuffer[LocalDate]()

    for (twa <- temporaryWorkingAgreementWithDates) {

      datePoints += twa.startDate

      // now get all the closed period that are in this TWA and pass that into the next for loop
      val newClosedList = sortedBusinessClosed(getAllClosedPeriodsWhichOverlapTwaPeriod(twa, businessClosedWithDates))

      for (bc <- newClosedList) {

        datePoints += bc.startDate.minusDays(1)
        datePoints += bc.endDate.plusDays(1)

      }
      datePoints += twa.endDate
    }
    // now we have all the data points to construct a list of TemporaryWorkingDates wich represent the open periods with surgeyr
    val openDates: List[TemporaryWorkingAgreementWithDates] =
      datePoints.toList.grouped(2).map(d => TemporaryWorkingAgreementWithDates(d.head, d.last)).toList

    openDates
  }

  def getAllBcsInThisPayPeriod(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): List[BusinessClosedWithDates] =
    businessClosedWithDates.filter(bc => isBCInPP(periodWithHours, bc))

  // this will get all the TWAs inside this PP but also will only return that portion of the TWA inside the PP, otherwise the splicing function won't work as expect as the dates for the TWA will be outside the PP
  def getAllTwasInThisPayPeriod(
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates],
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): List[TemporaryWorkingAgreementWithDates] =
    //Make sure that this PP is not completey covered by a Closed Period
    temporaryWorkingAgreementWithDates
      .filter(twa => isTwaInPP(periodWithHours, twa)) //TODO: check if this is not eliminiateing valid TWAs

  def getAllClosedPeriodsWhichOverlapTwaPeriod(
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates,
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): List[BusinessClosedWithDates] =
    businessClosedWithDates.filter(p => isClosedInTwa(temporaryWorkingAgreementWithDates, p))

  def isBCInPP(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: BusinessClosedWithDates
  ): Boolean =
    if (
      businessClosedWithDates.startDate.isEqual(
        businessClosedWithDates.endDate
      ) && businessClosedWithDates.startDate.isEqual(periodWithHours.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isEqual(periodWithHours.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isAfter(periodWithHours.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && businessClosedWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && (businessClosedWithDates.startDate.isBefore(periodWithHours.endDate) | businessClosedWithDates.startDate
        .isEqual(periodWithHours.endDate))
      && businessClosedWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && (businessClosedWithDates.startDate.isBefore(periodWithHours.endDate) | businessClosedWithDates.startDate
        .isEqual(periodWithHours.endDate))
      && businessClosedWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        periodWithHours.endDate
      ) && (businessClosedWithDates.startDate.isBefore(periodWithHours.endDate) | businessClosedWithDates.startDate
        .isEqual(periodWithHours.endDate))
      && businessClosedWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        periodWithHours.endDate
      ) && businessClosedWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else {
      false
    }

  def isClosedInTwa(
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates,
    businessClosedWithDates: BusinessClosedWithDates
  ): Boolean =
    if (
      businessClosedWithDates.startDate.isEqual(
        businessClosedWithDates.endDate
      ) && businessClosedWithDates.startDate.isEqual(temporaryWorkingAgreementWithDates.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.startDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isBefore(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isBefore(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else if (
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      true
    } else {
      false
    }

  def isTwaInPP(
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates
  ): Boolean =
    if ( //1
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && temporaryWorkingAgreementWithDates.startDate.isEqual(periodWithHours.startDate)
    ) {
      true
    } else if ( // 2
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.startDate)
    ) {
      true
    } else if ( // 3
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.startDate)
    ) {
      true
    } else if ( // 4
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if ( // 5
      temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if ( // 6
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      true
    } else if ( // 7
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if ( // 8
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.startDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if ( // 9
      (temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && (temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.endDate
      ) || temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.endDate
      )))

      && temporaryWorkingAgreementWithDates.endDate.isBefore(periodWithHours.endDate)
    ) {
      true
    } else if ( // 10
      (temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && (temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.endDate
      ) || temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.endDate
      )))

      && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      (temporaryWorkingAgreementWithDates.startDate.isAfter(
        periodWithHours.startDate
      ) && (temporaryWorkingAgreementWithDates.startDate.isBefore(
        periodWithHours.endDate
      ) || temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.endDate
      )))

      && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.endDate
      ) && temporaryWorkingAgreementWithDates.endDate.isEqual(periodWithHours.endDate)
    ) {
      true
    } else if (
      temporaryWorkingAgreementWithDates.startDate.isEqual(
        periodWithHours.endDate
      ) && temporaryWorkingAgreementWithDates.endDate.isAfter(periodWithHours.endDate)
    ) {
      true
    } else {
      false
    }

  def calculateNumberOfTwaDaysInPayPeriod(
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates
  ): Int =
    /*
      there can be a number of different configurations
     */
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

  def totalNumberOfTwaDaysInPayPeriod(
    periodWithHours: PeriodWithHours,
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates]
  ): Int = {
    //TODO: assumes that the list is sorted - ensure that
    val twaInPeriod = temporaryWorkingAgreementWithDates.filter(twa => isTwaInPP(periodWithHours, twa))
    twaInPeriod.map(twa => calculateNumberOfTwaDaysInPayPeriod(periodWithHours, twa)).sum
  }

  //TODO: remove
  def businessClosedPeriodCompletelyOverlapsPayPeriod(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: BusinessClosedWithDates
  ): Boolean =
    if (
      (businessClosedWithDates.startDate
        .isBefore(periodWithHours.startDate) && businessClosedWithDates.endDate.isAfter(periodWithHours.endDate)) ||
      (businessClosedWithDates.startDate
        .isEqual(periodWithHours.startDate) && businessClosedWithDates.endDate.isEqual(periodWithHours.endDate))
    ) true
    else false

  def sortedTWA(in: List[TemporaryWorkingAgreementWithDates]): List[TemporaryWorkingAgreementWithDates] =
    in.sortWith((x, y) => x.endDate.isBefore(y.endDate))

  def sortedBusinessClosed(in: List[BusinessClosedWithDates]): List[BusinessClosedWithDates] =
    in.sortWith((x, y) => x.endDate.isBefore(y.endDate))

  private def isDateIntervalOverlapping(
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates,
    businessClosedWithDates: BusinessClosedWithDates
  ): Boolean =
    if (
      temporaryWorkingAgreementWithDates.startDate.isAfter(
        businessClosedWithDates.endDate
      ) || businessClosedWithDates.startDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) false
    else true

  def hasOverlappingTwaAndBusinessClosedPeriods(
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates],
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): Boolean =
    sortedTWA(temporaryWorkingAgreementWithDates).exists(twa =>
      sortedBusinessClosed(businessClosedWithDates).exists(bc => isDateIntervalOverlapping(twa, bc))
    )

  def hasOnlyTwaPeriods(businessClosedWithDates: List[BusinessClosedWithDates]): Boolean =
    businessClosedWithDates.isEmpty

  def getNumberOfDaysInPayFrequency(payFrequency: PayFrequency, periodWithHours: PeriodWithHours): Int =
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
    periodWithHours: PeriodWithHours,
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

  private def trialadjustedReferencePay(referencePay: Double, daysInPeriod: Int, qualifyingDaysInPeriod: Int): Double =
    referencePay * (daysInPeriod.toDouble / qualifyingDaysInPeriod.toDouble)

  private def calculateReferencePayCap(
    daysInPeriod: Int,
    isPartialPeriod: Boolean,
    payFrequency: PayFrequency
  ): Double =
    if (isPartialPeriod) daysInPeriod * 102.74
    else RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)

  private def capReferencePay(referencePay: Double, referencePayCap: Double): Double =
    scala.math.min(referencePay, referencePayCap)

  def calculateFrequencyDaysForMonthlyFrequency(periodWithHours: PeriodWithHours): Int =
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
