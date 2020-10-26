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
import models.PeriodGrant.OpenPeriodGrant
import models.{BusinessClosedWithDates, Grant, JobSupport, JobSupportClosed, JobSupportOpen, PayFrequency, PeriodWithHours, SupportClaimPeriod, TemporaryWorkingAgreementWithDates, UsualAndActualHours}
import services.RegularPayGrantCalculator.partialPeriodPayCaps

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.{RoundingMode, double2bigDecimal}

case class PeriodSupport(
  open: JobSupportOpen,
  closed: JobSupportClosed
)

/*
  //TODO: draft implementation of the calculator - needs refactoring and re-design
 */
object PeriodSupport {
  implicit class PeriodSupportOps(private val periodSupports: List[PeriodSupport]) {
    def totalEmployeeSalary: Double =
      periodSupports.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.salary)

    def totalEmployersGrant: Double = periodSupports.map(s => s.open).foldLeft(0.0)((acc, f) => acc + f.grant)

    def totalClosed: Double = periodSupports.map(s => s.closed).foldLeft(0.0)((acc, f) => acc + f.f)

    def totalGrant: Double = totalEmployersGrant + totalClosed
  }
}

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

    val results: List[PeriodSupport] = payPeriods.map { payPeriod =>
      //TODO: refactor: assess whether this pay period needs to run both calculation (efficiency)
      PeriodSupport(
        calculateJobSupportOpen(
          supportClaimPeriod,
          payPeriod,
          getAllTwasInThisPayPeriod(payPeriod, sortedTWAList, sortedBCList),
          sortedBCList,
          payFrequency,
          referencePay
        ),
        calculateJobSupportClosed(
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
      false, //FIXME
      results.totalEmployeeSalary,
      results.totalGrant
    )
  }

  def calculateJobSupportClosed(
    supportClaimPeriod: SupportClaimPeriod,
    periodWithHours: PeriodWithHours,
    bcs: List[BusinessClosedWithDates],
    payFrequency: PayFrequency,
    referencePay: Double
  ): JobSupportClosed = {

    //TODO: refactor
    val totalClosedDaysInPP = getTotalNumberOfClosedDaysInAPayPeriod(periodWithHours, bcs).toDouble

    // THis is the number of days a pay period is in the claim period
    val step2: Int = qualifyingClaimDays(periodWithHours, supportClaimPeriod)

    // This returns the number of days in the frequency
    val freqDays = daysInPeriod(payFrequency, periodWithHours)

    // The reference pay is adjusted to account for the number days in this pay period that span the claim period
    val step3 = adjustedReferencePay(referencePay, freqDays, step2)

    // This gets the cap to be applied and it is apportioned appropriately
    val step4 = referencePayCap(step2, !(freqDays == totalClosedDaysInPP), payFrequency)

    // Appyly the pay cap
    val step5 = applyCapToReferencePay(step3, step4)

    val closed = step5 * (totalClosedDaysInPP / step2) * 2.0 / 3.0

    JobSupportClosed(closed)

  }

  def getTotalNumberOfClosedDaysInAPayPeriod(
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

  //TODO: simplify + refactor
  def calculateJobSupportOpen(
    supportClaimPeriod: SupportClaimPeriod,
    periodWithHours: PeriodWithHours,
    twas: List[TemporaryWorkingAgreementWithDates],
    bcs: List[BusinessClosedWithDates],
    payFrequency: PayFrequency,
    referencePay: Double
  ): JobSupportOpen =
    if (twas.isEmpty) {
      JobSupportOpen(0.0, 0.0)
    } else if (ppCoveredByBC(periodWithHours, bcs)) {
      JobSupportOpen(0.0, 0.0)
    } else {
      (hasOnlyTwaPeriods(bcs), hasOverlappingTwaAndBusinessClosedPeriods(twas, bcs)) match {
        case (true, _) =>
          val numberOfDaysInTwa = totalNumberOfTwaDaysInPayPeriod(periodWithHours, twas)

          // THis is the number of days a pay period is in the claim period
          val step2: Int = qualifyingClaimDays(periodWithHours, supportClaimPeriod)

          // This returns the number of days in the frequency
          val freqDays = daysInPeriod(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, numberOfDaysInTwa, step2)

          // This gets the cap to be applied and it is apportioned appropriately
          val step4 = referencePayCap(numberOfDaysInTwa, !(freqDays == numberOfDaysInTwa), payFrequency)

          // Apply the pay cap
          val step5 = applyCapToReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          JobSupportOpen(
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )

        case (false, false) =>
          val s = totalNumberOfTwaDaysInPayPeriod(periodWithHours, twas)

          // This returns the number of days in the frequency
          val freqDays = daysInPeriod(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, s, freqDays)

          // This gets the cap to be applied and it is approptioned appropriately
          val step4 = referencePayCap(s, !(freqDays == s), payFrequency)

          // Appyly the pay cap
          val step5 = applyCapToReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          JobSupportOpen(
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )

        case (false, true) =>
          val adjustedTwaList = getAllTwasInThisPayPeriod(periodWithHours, twas, bcs)

          val newAdjustedTwas = twaSurgery(adjustedTwaList, bcs)

          val s = totalNumberOfTwaDaysInPayPeriod(periodWithHours, newAdjustedTwas)

          // This returns the number of days in the frequency
          val freqDays = daysInPeriod(payFrequency, periodWithHours)

          // The reference pay is adjusted to account for the number days in this pay period that span the claim period
          val step3 = trialadjustedReferencePay(referencePay, s, freqDays)

          // This gets the cap to be applied and it is approptioned appropriately
          val step4 = referencePayCap(s, !(freqDays == s), payFrequency)

          // Appyly the pay cap
          val step5 = applyCapToReferencePay(step3, step4)

          val employeeSalary =
            step5 * ((periodWithHours.usualHours - periodWithHours.actualHours) / periodWithHours.usualHours) * 0.6667

          val employersGrant = employeeSalary * (61.67 / 66.67)

          JobSupportOpen(
            BigDecimal(employeeSalary).setScale(2, RoundingMode.UP).toDouble,
            BigDecimal(employersGrant).setScale(2, RoundingMode.UP).toDouble
          )

      }
    }

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

  def hasBCInThisPP(
    periodWithHours: PeriodWithHours,
    businessClosedWithDates: List[BusinessClosedWithDates]
  ): Boolean =
    businessClosedWithDates.exists(bc => isBCInPP(periodWithHours, bc))

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

  def spliceTWA(
    temporaryWorkingAgreementWithDates: TemporaryWorkingAgreementWithDates,
    businessClosedWithDates: BusinessClosedWithDates
  ): List[TemporaryWorkingAgreementWithDates] =
    if ( //1
      businessClosedWithDates.startDate.isEqual(businessClosedWithDates.endDate) && businessClosedWithDates.startDate
        .isEqual(temporaryWorkingAgreementWithDates.startDate)
    )
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate.plusDays(1),
          temporaryWorkingAgreementWithDates.endDate
        )
      )
    else if ( //2
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.startDate)
    )
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate.plusDays(1),
          temporaryWorkingAgreementWithDates.endDate
        )
      )
    else if ( // 3
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.startDate)
    )
      List(
        TemporaryWorkingAgreementWithDates(
          businessClosedWithDates.endDate.plusDays(1),
          temporaryWorkingAgreementWithDates.endDate
        )
      )
    else if ( // 4
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List[TemporaryWorkingAgreementWithDates]()
    } else if ( // 5
      businessClosedWithDates.startDate.isBefore(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List[TemporaryWorkingAgreementWithDates]()
    } else if ( // 6
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isBefore(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          businessClosedWithDates.startDate.plusDays(1),
          temporaryWorkingAgreementWithDates.endDate
        )
      )
    } else if ( // 7
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List[TemporaryWorkingAgreementWithDates]()
    } else if ( // 8
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List[TemporaryWorkingAgreementWithDates]()
    } else if ( // 9
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isBefore(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate,
          businessClosedWithDates.startDate.minusDays(1)
        ),
        TemporaryWorkingAgreementWithDates(
          businessClosedWithDates.endDate.plusDays(1),
          temporaryWorkingAgreementWithDates.endDate
        )
      )
    } else if ( // 10
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate,
          businessClosedWithDates.startDate.minusDays(1)
        )
      )
    } else if ( // 11
      businessClosedWithDates.startDate.isAfter(
        temporaryWorkingAgreementWithDates.startDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate,
          businessClosedWithDates.startDate.minusDays(1)
        )
      )
    } else if ( // 12
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && businessClosedWithDates.endDate.isEqual(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate,
          businessClosedWithDates.startDate.minusDays(1)
        )
      )
    } else if ( // 13
      businessClosedWithDates.startDate.isEqual(
        temporaryWorkingAgreementWithDates.endDate
      ) && businessClosedWithDates.endDate.isAfter(temporaryWorkingAgreementWithDates.endDate)
    ) {
      List(
        TemporaryWorkingAgreementWithDates(
          temporaryWorkingAgreementWithDates.startDate,
          businessClosedWithDates.startDate.minusDays(1)
        )
      )
    } else {
      List[TemporaryWorkingAgreementWithDates]()
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

  private def sortedTWA(in: List[TemporaryWorkingAgreementWithDates]): List[TemporaryWorkingAgreementWithDates] =
    in.sortWith((x, y) => x.endDate.isBefore(y.endDate))

  private def sortedBusinessClosed(in: List[BusinessClosedWithDates]): List[BusinessClosedWithDates] =
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

  def hasOnlyBusinessClosedPeriods(
    temporaryWorkingAgreementWithDates: List[TemporaryWorkingAgreementWithDates]
  ): Boolean =
    temporaryWorkingAgreementWithDates.isEmpty

  def hoursNotWorked(usualAndActualHours: UsualAndActualHours): Double =
    usualAndActualHours.usualHours - usualAndActualHours.actualHours

  def nonWorkingHoursPay(
    adjustedReferencePay: Double,
    numberOfTwaDaysInPeriod: Int,
    qualifyingClaimDaysInPeriod: Int,
    numberOfHoursNotWorked: Double,
    usualHoursWorked: Double
  ): Double =
    adjustedReferencePay * (numberOfTwaDaysInPeriod.toDouble / qualifyingClaimDaysInPeriod.toDouble) * (numberOfHoursNotWorked.toDouble / usualHoursWorked.toDouble) * 0.6667

  def grantClaimAmount(nonWorkingHoursPay: Double): Double = nonWorkingHoursPay * (61.67 / 66.67)

  def closedSchemeGrant(
    adjustReferencePay: Double,
    numberOfClosedDaysInPeriod: Int,
    numberOfQualifyingDaysInPeriod: Int
  ): Double =
    adjustReferencePay * (numberOfClosedDaysInPeriod / numberOfQualifyingDaysInPeriod) * (2.0 / 3.0)

  def applyReferencePayCap(referencePay: Double, referencePayCap: Double): Double =
    scala.math.min(referencePay, referencePayCap)

  def daysInPeriod(payFrequency: PayFrequency, periodWithHours: PeriodWithHours): Int =
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
  def qualifyingClaimDays(periodWithHours: PeriodWithHours, supportClaimPeriod: SupportClaimPeriod): Int =
    if (
      supportClaimPeriod.startDate
        .isAfter(periodWithHours.endDate) || periodWithHours.startDate.isAfter(supportClaimPeriod.endDate)
    ) 0
    else {
      val start = scala.math.max(supportClaimPeriod.startDate.toEpochDay, periodWithHours.startDate.toEpochDay)
      val end   = scala.math.min(supportClaimPeriod.endDate.toEpochDay, periodWithHours.endDate.toEpochDay)
      ChronoUnit.DAYS.between(LocalDate.ofEpochDay(start), LocalDate.ofEpochDay(end)).toInt + 1
    }

  private def adjustedReferencePay(referencePay: Double, daysInPeriod: Int, qualifyingDaysInPeriod: Int): Double =
    (referencePay / daysInPeriod.toDouble) * qualifyingDaysInPeriod.toDouble

  private def trialadjustedReferencePay(referencePay: Double, daysInPeriod: Int, qualifyingDaysInPeriod: Int): Double =
    referencePay * (daysInPeriod.toDouble / qualifyingDaysInPeriod.toDouble)

  private def isPartialPeriod(periodWithHours: PeriodWithHours, supportClaimPeriod: SupportClaimPeriod): Boolean =
    periodWithHours.startDate.isBefore(supportClaimPeriod.startDate)

  private def referencePayCap(daysInPeriod: Int, isPartialPeriod: Boolean, payFrequency: PayFrequency): Double =
    if (isPartialPeriod) daysInPeriod * 102.74
    else RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)

  private def applyCapToReferencePay(referencePay: Double, referencePayCap: Double): Double =
    scala.math.min(referencePay, referencePayCap)

  def calculateRegularPayGrant(
    periods: List[PeriodWithHours],
    referencePay: BigDecimal,
    supportClaimPeriod: SupportClaimPeriod,
    payFrequency: PayFrequency
  ): Grant = {
    val grantForPeriods: List[OpenPeriodGrant] = periods.map { period =>
      if (isPartialPeriod(period, supportClaimPeriod))
        calculateGrantForPartialPeriod(referencePay, period, supportClaimPeriod, payFrequency)
      else {
        calculateGrantForFullPeriod(referencePay, period, supportClaimPeriod, payFrequency)
      }
    }
    Grant(
      grantForPeriods,
      referencePay,
      isEligibleForGrant(grantForPeriods),
      grantForPeriods.foldLeft(0.0)((acc, d) => acc + d.grant.doubleValue())
    )
  }

  def isEligibleForGrant(grantPeriods: List[OpenPeriodGrant]): Boolean = {
    val totalUsualHours  = grantPeriods.foldLeft(0.0)((acc, grantPeriod) => acc + grantPeriod.period.usualHours)
    val totalActualHours = grantPeriods.foldLeft(0.0)((acc, grantPeriod) => acc + grantPeriod.period.actualHours)
    val oneThird         = BigDecimal("0.33")

    if (BigDecimal(totalActualHours / totalUsualHours).compareTo(oneThird) == 1)
      true
    else
      false
  }

  def calculateGrantForPartialPeriod(
    referencePay: BigDecimal,
    period: PeriodWithHours,
    supportClaimPeriod: SupportClaimPeriod,
    payFrequency: PayFrequency
  ): OpenPeriodGrant = {
    val daysInFrequency      = payFrequency match {
      case PayFrequency.Weekly      => PayFrequency.payFrequencyDays(Weekly)
      case PayFrequency.FortNightly => PayFrequency.payFrequencyDays(FortNightly)
      case PayFrequency.FourWeekly  => PayFrequency.payFrequencyDays(FourWeekly)
      case PayFrequency.Monthly     => calculateFrequencyDaysForMonthlyFrequency(period)
    }
    val daysInPartialPeriod  = calculateEligibleDaysForClaim(period.endDate, supportClaimPeriod, daysInFrequency) + 1
    val claimMonth           = supportClaimPeriod.startDate.getMonth
    val payCap               = partialPeriodPayCaps.getOrElse(claimMonth, 0.0)
    val referencePayCap      = daysInPartialPeriod * payCap
    val adjustedReferencePay = referencePay.doubleValue() * (daysInPartialPeriod.toDouble / daysInFrequency.toDouble)
    val actualReferencePay   = scala.math.min(adjustedReferencePay, referencePayCap)
    val grant: BigDecimal    =
      ((actualReferencePay * ((period.usualHours - period.actualHours) / period.usualHours)) / 3.0)
        .setScale(2, RoundingMode.HALF_UP)
    OpenPeriodGrant(
      period,
      grant.doubleValue(),
      daysInPartialPeriod.toInt,
      daysInFrequency,
      payCap.toInt, //FIXME
      actualReferencePay,
      referencePay.toDouble,
      payFrequency,
      true,
      0
    ) //FIXME
  }

  def calculateGrantForFullPeriod(
    referencePay: BigDecimal,
    period: PeriodWithHours,
    supportClaimPeriod: SupportClaimPeriod,
    payFrequency: PayFrequency
  ): OpenPeriodGrant = {
    val daysInFrequency      = payFrequency match {
      case PayFrequency.Weekly      => PayFrequency.payFrequencyDays(Weekly)
      case PayFrequency.FortNightly => PayFrequency.payFrequencyDays(FortNightly)
      case PayFrequency.FourWeekly  => PayFrequency.payFrequencyDays(FourWeekly)
      case PayFrequency.Monthly     => calculateFrequencyDaysForMonthlyFrequency(period)
    }
    val daysInPartialPeriod  = calculateEligibleDaysForClaim(period.endDate, supportClaimPeriod, daysInFrequency) + 1
    val referencePayCap      = RegularPayGrantCalculator.fullPeriodPayCaps.getOrElse(payFrequency, 0.0)
    val adjustedReferencePay = referencePay.doubleValue()
    val actualReferencePay   = scala.math.min(adjustedReferencePay, referencePayCap)
    val grant: BigDecimal    =
      ((actualReferencePay * ((period.usualHours - period.actualHours) / period.usualHours)) / 3.0)
        .setScale(2, RoundingMode.HALF_UP)
    OpenPeriodGrant(
      period,
      grant.doubleValue(),
      daysInPartialPeriod.toInt,
      daysInPartialPeriod.toInt,
      referencePayCap.toInt, //FIXME
      actualReferencePay,
      referencePay.toDouble, //FIXME
      payFrequency,
      false,
      0
    ) //FIME
  }

  def calculateEligibleDaysForClaim(
    periodEndDate: LocalDate,
    supportClaimPeriod: SupportClaimPeriod,
    payFrequencyDays: Int
  ): Long =
    scala.math.min(ChronoUnit.DAYS.between(supportClaimPeriod.startDate, periodEndDate), payFrequencyDays)

  def calculateFrequencyDaysForMonthlyFrequency(periodWithHours: PeriodWithHours): Int =
    ChronoUnit.DAYS.between(periodWithHours.startDate, periodWithHours.endDate).toInt + 1
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
