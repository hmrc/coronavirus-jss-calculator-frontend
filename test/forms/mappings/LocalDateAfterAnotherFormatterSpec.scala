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

import generators.Generators
import models.StartAndEndDate
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import play.api.data.{Form, FormError}

class LocalDateAfterAnotherFormatterSpec
    extends FreeSpec
    with MustMatchers
    with ScalaCheckPropertyChecks
    with Generators
    with OptionValues
    with Mappings {

  case class TestPeriod(startDate: LocalDate, endDate: LocalDate) extends StartAndEndDate

  val existingPeriods = Seq(
    TestPeriod(LocalDate.of(2020, 11, 1), LocalDate.of(2020, 11, 7))
  )

  val form = Form(
    "endDate" -> localDateAfterAnother(
      otherField = "startDate",
      otherPeriods = existingPeriods,
      minimumDaysBetween = 6,
      invalidKey = "invalidKey",
      requiredKey = "requiredKey",
      mustBeAfterKey = "mustBeAfterKey",
      daysBetweenKey = "daysBetweenKey",
      mustNotOverlapKey = "mustNotOverlapKey",
      args = Seq("foo")
    )
  )

  val validData = datesBetween(
    min = LocalDate.of(2020, 11, 8),
    max = LocalDate.of(3000, 1, 1)
  )

  val invalidField: Gen[String] = Gen.alphaStr.suchThat(_.nonEmpty)

  val missingField: Gen[Option[String]] = Gen.option(Gen.const(""))

  "LocalDateAfterAnotherFormatter" - {

    "must bind valid data" in {

      val data = Map(
        "startDate.day"   -> "8",
        "startDate.month" -> "11",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "14",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.value.value mustEqual LocalDate.of(2020, 11, 14)
    }

    "must fail to bind an empty date" in {

      val result = form.bind(Map.empty[String, String])

      result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
    }

    "fail to bind a date with a missing day" in {

      forAll(validData -> "valid date", missingField -> "missing field") { (date, field) =>
        val initialData = Map(
          "endDate.month" -> date.getMonthValue.toString,
          "endDate.year"  -> date.getYear.toString
        )

        val data = field.fold(initialData) { endDate =>
          initialData + ("endDate.day" -> endDate)
        }

        val result = form.bind(data)

        result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind a date with an invalid day" in {

      forAll(validData -> "valid date", invalidField -> "invalid field") { (date, field) =>
        val data = Map(
          "endDate.day"   -> field,
          "endDate.month" -> date.getMonthValue.toString,
          "endDate.year"  -> date.getYear.toString
        )

        val result = form.bind(data)

        result.errors must contain(
          FormError("endDate", "invalidKey", Seq("foo"))
        )
      }
    }

    "fail to bind a date with a missing month" in {

      forAll(validData -> "valid date", missingField -> "missing field") { (date, field) =>
        val initialData = Map(
          "endDate.day"  -> date.getDayOfMonth.toString,
          "endDate.year" -> date.getYear.toString
        )

        val data = field.fold(initialData) { endDate =>
          initialData + ("endDate.month" -> endDate)
        }

        val result = form.bind(data)

        result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind a date with an invalid month" in {

      forAll(validData -> "valid data", invalidField -> "invalid field") { (date, field) =>
        val data = Map(
          "endDate.day"   -> date.getDayOfMonth.toString,
          "endDate.month" -> field,
          "endDate.year"  -> date.getYear.toString
        )

        val result = form.bind(data)

        result.errors must contain(
          FormError("endDate", "invalidKey", Seq("foo"))
        )
      }
    }

    "fail to bind a date with a missing year" in {

      forAll(validData -> "valid date", missingField -> "missing field") { (date, field) =>
        val initialData = Map(
          "endDate.day"   -> date.getDayOfMonth.toString,
          "endDate.month" -> date.getMonthValue.toString
        )

        val data = field.fold(initialData) { endDate =>
          initialData + ("endDate.year" -> endDate)
        }

        val result = form.bind(data)

        result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind a date with an invalid year" in {

      forAll(validData -> "valid data", invalidField -> "invalid field") { (date, field) =>
        val data = Map(
          "endDate.day"   -> date.getDayOfMonth.toString,
          "endDate.month" -> date.getMonthValue.toString,
          "endDate.year"  -> field
        )

        val result = form.bind(data)

        result.errors must contain(
          FormError("endDate", "invalidKey", Seq("foo"))
        )
      }
    }

    "fail to bind a date with a missing day and month" in {

      forAll(validData -> "valid date", missingField -> "missing day", missingField -> "missing month") {
        (date, dayOpt, monthOpt) =>
          val day = dayOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.day" -> endDate)
          }

          val month = monthOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.month" -> endDate)
          }

          val data: Map[String, String] = Map(
            "endDate.year" -> date.getYear.toString
          ) ++ day ++ month

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind a date with a missing day and year" in {

      forAll(validData -> "valid date", missingField -> "missing day", missingField -> "missing year") {
        (date, dayOpt, yearOpt) =>
          val day = dayOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.day" -> endDate)
          }

          val year = yearOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.year" -> endDate)
          }

          val data: Map[String, String] = Map(
            "endDate.month" -> date.getMonthValue.toString
          ) ++ day ++ year

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind a date with a missing month and year" in {

      forAll(validData -> "valid date", missingField -> "missing month", missingField -> "missing year") {
        (date, monthOpt, yearOpt) =>
          val month = monthOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.month" -> endDate)
          }

          val year = yearOpt.fold(Map.empty[String, String]) { endDate =>
            Map("endDate.year" -> endDate)
          }

          val data: Map[String, String] = Map(
            "endDate.day" -> date.getDayOfMonth.toString
          ) ++ month ++ year

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "requiredKey", Seq("foo"))
      }
    }

    "fail to bind an invalid day and month" in {

      forAll(validData -> "valid date", invalidField -> "invalid day", invalidField -> "invalid month") {
        (date, day, month) =>
          val data = Map(
            "endDate.day"   -> day,
            "endDate.month" -> month,
            "endDate.year"  -> date.getYear.toString
          )

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "invalidKey", Seq("foo"))
      }
    }

    "fail to bind an invalid day and year" in {

      forAll(validData -> "valid date", invalidField -> "invalid day", invalidField -> "invalid year") {
        (date, day, year) =>
          val data = Map(
            "endDate.day"   -> day,
            "endDate.month" -> date.getMonthValue.toString,
            "endDate.year"  -> year
          )

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "invalidKey", Seq("foo"))
      }
    }

    "fail to bind an invalid month and year" in {

      forAll(validData -> "valid date", invalidField -> "invalid month", invalidField -> "invalid year") {
        (date, month, year) =>
          val data = Map(
            "endDate.day"   -> date.getDayOfMonth.toString,
            "endDate.month" -> month,
            "endDate.year"  -> year
          )

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "invalidKey", Seq("foo"))
      }
    }

    "fail to bind an invalid day, month and year" in {

      forAll(invalidField -> "valid day", invalidField -> "invalid month", invalidField -> "invalid year") {
        (day, month, year) =>
          val data = Map(
            "endDate.day"   -> day,
            "endDate.month" -> month,
            "endDate.year"  -> year
          )

          val result = form.bind(data)

          result.errors must contain only FormError("endDate", "invalidKey", Seq("foo"))
      }
    }

    "fail to bind an invalid date" in {

      val data = Map(
        "endDate.day"   -> "30",
        "endDate.month" -> "2",
        "endDate.year"  -> "2018"
      )

      val result = form.bind(data)

      result.errors must contain(
        FormError("endDate", "invalidKey", Seq("foo"))
      )
    }

    "fail to bind a date that's the same as the other date" in {

      val data = Map(
        "startDate.day"   -> "8",
        "startDate.month" -> "11",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "8",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.errors must contain only FormError("endDate", "mustBeAfterKey", Seq("foo"))
    }

    "fail to bind a date that's before the other date" in {

      val data = Map(
        "startDate.day"   -> "8",
        "startDate.month" -> "11",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "7",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.errors must contain only FormError("endDate", "mustBeAfterKey", Seq("foo"))
    }

    "fail to bind a date that's less than the minimum number of days after the other date" in {

      val data = Map(
        "startDate.day"   -> "8",
        "startDate.month" -> "11",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "13",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.errors must contain only FormError("endDate", "daysBetweenKey", Seq("foo"))
    }

    "fail to bind a date that makes this period overlap another" in {

      val data = Map(
        "startDate.day"   -> "1",
        "startDate.month" -> "10",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "1",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.errors must contain only FormError("endDate", "mustNotOverlapKey", Seq("foo"))
    }

    "fail to bind when the other date makes this period overlap another" in {

      val data = Map(
        "startDate.day"   -> "6",
        "startDate.month" -> "11",
        "startDate.year"  -> "2020",
        "endDate.day"     -> "13",
        "endDate.month"   -> "11",
        "endDate.year"    -> "2020"
      )

      val result = form.bind(data)

      result.errors must contain only FormError("endDate", "mustNotOverlapKey", Seq("foo"))
    }

    "unbind a date" in {

      forAll(validData -> "valid date") { date =>
        val filledForm = form.fill(date)

        filledForm("endDate.day").value.value mustEqual date.getDayOfMonth.toString
        filledForm("endDate.month").value.value mustEqual date.getMonthValue.toString
        filledForm("endDate.year").value.value mustEqual date.getYear.toString
      }
    }
  }
}
