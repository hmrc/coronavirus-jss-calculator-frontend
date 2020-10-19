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

package base

import config.FrontendAppConfig
import controllers.actions._
import handlers.ErrorHandler
import models.UserAnswers
import navigation.Navigator
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatest.TryValues
import org.scalatestplus.mockito.MockitoSugar
import org.scalatestplus.play.PlaySpec
import org.scalatestplus.play.guice._
import play.api.Configuration
import play.api.i18n.{Messages, MessagesApi}
import play.api.inject.Injector
import play.api.mvc.{AnyContentAsEmpty, MessagesControllerComponents}
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import repositories.SessionRepository

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

trait SpecBaseControllerSpecs extends PlaySpec with GuiceOneAppPerSuite with MockitoSugar with TryValues {

  val userAnswersId: String = "id"
  val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  val injector: Injector = app.injector
  def messagesApi = injector.instanceOf[MessagesApi]
  val component = injector.instanceOf[MessagesControllerComponents]
  val getSessionAction = injector.instanceOf[FakeGetSessionAction]
  val dataRequired: DataRequiredAction = injector.instanceOf[DataRequiredActionImpl]
  val navigator = injector.instanceOf[Navigator]
  val dataRetrieval: DataRetrievalAction = new DataRetrievalActionImpl(mockSessionRepository)
  val configuration = injector.instanceOf[Configuration]
  implicit val errorHandler: ErrorHandler = injector.instanceOf[ErrorHandler]
  implicit val frontendAppConfig: FrontendAppConfig = new FrontendAppConfig(configuration)

  def fakeRequest(method: String = "", path: String = ""): FakeRequest[AnyContentAsEmpty.type] =
    FakeRequest(method, path).withCSRFToken.asInstanceOf[FakeRequest[AnyContentAsEmpty.type]]

  implicit val messages: Messages = messagesApi.preferred(fakeRequest())

  lazy val mockSessionRepository: SessionRepository = {
    val mockSession = mock[SessionRepository]
    when(mockSession.set(any())) thenReturn Future.successful(true)
    when(mockSession.clear(any())) thenReturn Future.successful(true)
    mockSession
  }

  val stubDataRetrieval: Option[UserAnswers] => DataRetrievalAction = stubbedAnswer => new FakeDataRetrievalAction(stubbedAnswer)

}
