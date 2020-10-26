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
import java.time.temporal.ChronoUnit

import play.api.data.format.Formats.stringFormat
import play.api.data.format.Formatter
import play.api.data.{Form, FormError}
import play.api.i18n.Messages
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.govukfrontend.views.viewmodels.checkboxes.CheckboxItem
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import views.ViewUtils.{dateToString, dateToStringWithoutYear}

import scala.util.{Failure, Success, Try}

final case class Period(
  startDate: LocalDate,
  endDate: LocalDate
)

object Period {
  implicit val defaultFormat: Format[Period] = Json.format
  implicit class Counter(period: Period) {
    def countDays: Int =
      (ChronoUnit.DAYS.between(period.startDate, period.endDate) + 1).toInt

    def countHours: Int = countDays * 24
  }

  implicit val formatter: Formatter[Period] = new Formatter[Period] {
    override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], Period] =
      stringFormat.bind(key, data).right.flatMap { wp =>
        Try {
          val startDate = LocalDate.parse(wp.split("_")(0))
          val endDate   = LocalDate.parse(wp.split("_")(1))
          Period(startDate, endDate)
        } match {
          case Success(period) => Right(period)
          case Failure(ex)     => Left(Seq(FormError(key, ex.getMessage)))
        }
      }

    override def unbind(key: String, workPeriod: Period): Map[String, String] = {
      val value = workPeriod.startDate.toString + "_" + workPeriod.endDate.toString
      Map(key -> value)
    }
  }

  def options(form: Form[_], periods: List[Period])(implicit messages: Messages): Seq[CheckboxItem] =
    periods.zipWithIndex.map { value =>
      val periodStart        = value._1.startDate
      val periodEnd          = value._1.endDate
      val selectedWorkPeriod = periodStart.toString + "_" + periodEnd.toString

      CheckboxItem(
        name = Some(s"value[${value._2}]"),
        id = Some(s"select-work-periods_${value._2.toString}"),
        value = selectedWorkPeriod,
        content = Text(
          messages(
            "selectWorkPeriods.from.to",
            dateToStringWithoutYear(value._1.startDate),
            dateToString(value._1.endDate)
          )
        ),
        checked = form.data.values.contains(selectedWorkPeriod)
      )
    }

  def toPeriodWithHours(period: Period, usualAndActualHours: UsualAndActualHours): PeriodWithHours =
    PeriodWithHours(period.startDate, period.endDate, usualAndActualHours.usualHours, usualAndActualHours.actualHours)
}
