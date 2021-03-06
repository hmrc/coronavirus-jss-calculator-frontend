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

package generators

import models.UserAnswers
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.TryValues
import pages._
import play.api.libs.json.{JsValue, Json}

trait UserAnswersGenerator extends TryValues {
  self: Generators =>

  val generators: Seq[Gen[(QuestionPage[_], JsValue)]] =
    arbitrary[(BusinessClosedPeriodsPage.type, JsValue)] ::
      arbitrary[(ShortTermWorkingAgreementPeriodPage.type, JsValue)] ::
      arbitrary[(BusinessClosedPage.type, JsValue)] ::
      arbitrary[(TemporaryWorkingAgreementPage.type, JsValue)] ::
      arbitrary[(EndPayDatePage.type, JsValue)] ::
      arbitrary[(UsualAndActualHoursPage.type, JsValue)] ::
      arbitrary[(RegularPayAmountPage.type, JsValue)] ::
      arbitrary[(RegularPayAmountPage.type, JsValue)] ::
      arbitrary[(PayPeriodsPage.type, JsValue)] ::
      arbitrary[(LastPayDatePage.type, JsValue)] ::
      arbitrary[(SelectWorkPeriodsPage.type, JsValue)] ::
      arbitrary[(PayFrequencyPage.type, JsValue)] ::
      arbitrary[(ClaimPeriodPage.type, JsValue)] ::
      arbitrary[(PayMethodPage.type, JsValue)] ::
      arbitrary[(ClaimPeriodPage.type, JsValue)] ::
      arbitrary[(ClaimPeriodPage.type, JsValue)] ::
      Nil

  implicit lazy val arbitraryUserData: Arbitrary[UserAnswers] = {

    import models._

    Arbitrary {
      for {
        id   <- nonEmptyString
        data <- generators match {
                  case Nil => Gen.const(Map[QuestionPage[_], JsValue]())
                  case _   => Gen.mapOf(oneOf(generators))
                }
      } yield UserAnswers(
        id = id,
        data = data.foldLeft(Json.obj()) { case (obj, (path, value)) =>
          obj.setObject(path.path, value).get
        }
      )
    }
  }
}
