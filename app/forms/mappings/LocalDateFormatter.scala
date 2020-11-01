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

package forms.mappings

import java.time.LocalDate
import java.time.temporal.ChronoUnit

import models.StartAndEndDate
import play.api.data.FormError
import play.api.data.format.Formatter

import scala.util.{Failure, Success, Try}

private[mappings] class LocalDateFormatter(
  invalidKey: String,
  requiredKey: String,
  args: Seq[String] = Seq.empty
) extends Formatter[LocalDate]
    with Formatters {

  protected val fieldKeys: List[String] = List("day", "month", "year")

  protected def toDate(key: String, day: Int, month: Int, year: Int): Either[Seq[FormError], LocalDate] =
    Try(LocalDate.of(year, month, day)) match {
      case Success(date) =>
        Right(date)
      case Failure(_)    =>
        Left(Seq(FormError(key, invalidKey, args)))
    }

  protected def formatDate(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {

    val int = intFormatter(
      requiredKey = invalidKey,
      wholeNumberKey = invalidKey,
      nonNumericKey = invalidKey,
      args
    )

    for {
      day   <- int.bind(s"$key.day", data).right
      month <- int.bind(s"$key.month", data).right
      year  <- int.bind(s"$key.year", data).right
      date  <- toDate(key, day, month, year).right
    } yield date
  }

  override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {

    val fields = fieldKeys.map { field =>
      field -> data.get(s"$key.$field").filter(_.nonEmpty)
    }.toMap

    fields.count(_._2.isDefined) match {
      case 3 =>
        formatDate(key, data).left.map {
          _.map(_.copy(key = key, args = args))
        }
      case _ =>
        Left(List(FormError(key, requiredKey, args)))
    }
  }

  override def unbind(key: String, value: LocalDate): Map[String, String] =
    Map(
      s"$key.day"   -> value.getDayOfMonth.toString,
      s"$key.month" -> value.getMonthValue.toString,
      s"$key.year"  -> value.getYear.toString
    )
}

private[mappings] class LocalDateAfterAnotherFormatter(
  otherField: String,
  otherPeriods: Seq[StartAndEndDate],
  minimumDaysBetween: Int,
  invalidKey: String,
  requiredKey: String,
  mustBeAfterKey: String,
  daysBetweenKey: String,
  mustNotOverlapKey: String,
  args: Seq[String] = Nil
) extends LocalDateFormatter(invalidKey, requiredKey, args) {

  private def dateInteractsWithPeriod(date: LocalDate, period: StartAndEndDate): Boolean =
    date.compareTo(period.startDate) >= 0 && date.compareTo(period.endDate) <= 0

  private def datesInteractWithOtherPeriods(thisDate: LocalDate, otherDate: LocalDate): Boolean =
    otherPeriods.exists(p => dateInteractsWithPeriod(thisDate, p) || dateInteractsWithPeriod(otherDate, p))

  private def checkDates(key: String, thisDate: LocalDate, otherDate: LocalDate): Either[List[FormError], LocalDate] =
    if (thisDate.compareTo(otherDate) <= 0) {
      Left(List(FormError(key, mustBeAfterKey, args)))
    } else if (ChronoUnit.DAYS.between(otherDate, thisDate) < minimumDaysBetween) {
      Left(List(FormError(key, daysBetweenKey, args)))
    } else if (datesInteractWithOtherPeriods(thisDate, otherDate)) {
      Left(List(FormError(key, mustNotOverlapKey, args)))
    } else {
      Right(thisDate)
    }

  override def bind(key: String, data: Map[String, String]): Either[Seq[FormError], LocalDate] = {

    val fields = fieldKeys.map { field =>
      field -> data.get(s"$key.$field").filter(_.nonEmpty)
    }.toMap

    if (fields.count(_._2.isDefined) == 3) {

      val thisDate = formatDate(key, data).left.map {
        _.map(_.copy(key = key, args = args))
      }

      val otherDate = formatDate(otherField, data)

      (thisDate, otherDate) match {
        case (Right(date), Right(other))                                 => checkDates(key, date, other)
        case (Left(errors), _) if errors.forall(_.message == invalidKey) => Left(List(FormError(key, invalidKey, args)))
        case (Left(_), _)                                                => Left(List(FormError(key, requiredKey, args)))
        case _                                                           => Left(Nil)
      }
    } else {
      Left(List(FormError(key, requiredKey, args)))
    }
  }
}
