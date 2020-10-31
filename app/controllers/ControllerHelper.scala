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

package controllers

import models.{NormalMode, UserAnswers}
import navigation.Navigator
import pages.QuestionPage
import play.api.mvc.Result
import play.api.mvc.Results._
import repositories.SessionRepository

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait ControllerHelper {

  val sessionRepository: SessionRepository
  val navigator: Navigator

  def saveAndRedirect[A](page: QuestionPage[A], userAnswers: Try[UserAnswers], idx: Option[Int] = None)(implicit
    ec: ExecutionContext
  ): Future[Result] =
    for {
      updatedAnswers <- Future.fromTry(userAnswers)
      _              <- sessionRepository.set(updatedAnswers)
    } yield Redirect(navigator.nextPage(page, NormalMode, updatedAnswers, idx))
}
