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

import java.time.YearMonth
import java.time.format.DateTimeFormatter

import models.ClaimPeriod.pattern
import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.radios.RadioItem

sealed trait ClaimPeriod {
  def key: String = getClass.getSimpleName.dropRight(1)
  def yearMonth: YearMonth = YearMonth.parse(toString, pattern)
  def supportClaimPeriod: SupportClaimPeriod = SupportClaimPeriod(yearMonth.atDay(1), yearMonth.atEndOfMonth())
}

object ClaimPeriod extends Enumerable.Implicits {

  val pattern: DateTimeFormatter = DateTimeFormatter.ofPattern("MMMM yyyy")

  case object Nov2020 extends WithName("November 2020") with ClaimPeriod
  case object Dec2020 extends WithName("December 2020") with ClaimPeriod
  case object Jan2021 extends WithName("January 2021") with ClaimPeriod
  case object Feb2021 extends WithName("February 2021") with ClaimPeriod
  case object Mar2021 extends WithName("March 2021") with ClaimPeriod
  case object Apr2021 extends WithName("April 2021") with ClaimPeriod
  case object May2021 extends WithName("May 2021") with ClaimPeriod
  case object Jun2021 extends WithName("June 2021") with ClaimPeriod
  case object Jul2021 extends WithName("July 2021") with ClaimPeriod
  case object Aug2021 extends WithName("August 2021") with ClaimPeriod
  case object Sep2021 extends WithName("September 2021") with ClaimPeriod
  case object Oct2021 extends WithName("October 2021") with ClaimPeriod
  case object Nov2021 extends WithName("November 2021") with ClaimPeriod

  val values: Seq[ClaimPeriod] = Seq(
    Nov2020,
    Dec2020,
    Jan2021,
    Feb2021,
    Mar2021,
    Apr2021,
    May2021,
    Jun2021,
    Jul2021,
    Aug2021,
    Sep2021,
    Oct2021,
    Nov2021
  )

  def options(form: Form[_], schemeEnds: String, currentMonthYear: YearMonth = YearMonth.now())(
    implicit messages: Messages): Seq[RadioItem] = {
    val nov2020 = YearMonth.parse("November 2020", pattern)
    val schemeEndsTime = YearMonth.parse(schemeEnds, pattern)

    if (currentMonthYear.compareTo(nov2020) <= 0) {
      Seq(
        RadioItem(
          value = Some(Nov2020.toString),
          content = Text(messages(s"claimPeriod.${Nov2020.key}")),
          checked = form("value").value.contains(Nov2020.key)
        ))
    } else {
      values
        .filter { value =>
          val ym = YearMonth.parse(value, pattern)
          //the date should be be on or before schemeEnds date && show only dates till current month && current date should be on or before schemeEnds date
          ym.compareTo(schemeEndsTime) <= 0 && ym.compareTo(currentMonthYear) <= 0 && currentMonthYear.compareTo(schemeEndsTime) <= 0
        }
        .map { value =>
          RadioItem(
            value = Some(value.toString),
            content = Text(messages(s"claimPeriod.${value.key}")),
            checked = form("value").value.contains(value.key)
          )
        }
    }

  }

  implicit val enumerable: Enumerable[ClaimPeriod] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
