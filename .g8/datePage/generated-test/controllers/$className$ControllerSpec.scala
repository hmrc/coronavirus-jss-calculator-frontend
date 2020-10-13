package controllers

import java.time.{LocalDate, ZoneOffset}

import base.SpecBase
import forms.$className$FormProvider
import models.{NormalMode, UserAnswers}
import navigation.{FakeNavigator, Navigator}
import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalatestplus.mockito.MockitoSugar
import pages.$className$Page
import play.api.inject.bind
import play.api.mvc.{AnyContentAsFormUrlEncoded, Call}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import repositories.SessionRepository
import views.html.$className$View

import scala.concurrent.Future

class $className$ControllerSpec extends SpecBase with MockitoSugar {

  private val formProvider = new $className$FormProvider()
  private def form = formProvider()

  private def onwardRoute = Call("GET", "/foo")

  private val validAnswer = LocalDate.now(ZoneOffset.UTC)

  private lazy val $className;format="decap"$Route = routes.$className$Controller.onPageLoad(NormalMode).url

  override val emptyUserAnswers: UserAnswers = UserAnswers(userAnswersId)

  private lazy val getRequest: FakeRequest[AnyContentAsEmpty.type] =
    fakeRequest(GET, $className;format="decap"$Route)

  private lazy val postRequest: FakeRequest[AnyContentAsFormUrlEncoded] =
    fakeRequest(POST, $className;format="decap"$Route)
      .withFormUrlEncodedBody(
        "value.day"   -> validAnswer.getDayOfMonth.toString,
        "value.month" -> validAnswer.getMonthValue.toString,
        "value.year"  -> validAnswer.getYear.toString
      )

  "$className$ Controller" must {

    "return OK and the correct view for a GET" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {

        val result = route(application, getRequest).value

        val view = application.injector.instanceOf[$className$View]

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form, NormalMode)(getRequest, messages(application)).toString
      }
    }

    "populate the view correctly on a GET when the question has previously been answered" in {

      val userAnswers = UserAnswers(userAnswersId).set($className$Page, validAnswer).success.value

      val application = applicationBuilder(userAnswers = Some(userAnswers)).build()

      running(application) {

        val view = application.injector.instanceOf[$className$View]

        val result = route(application, getRequest).value

        status(result) mustEqual OK

        contentAsString(result) mustEqual
          view(form.fill(validAnswer), NormalMode)(getRequest, messages(application)).toString
      }
    }

    "redirect to the next page when valid data is submitted" in {

      val mockSessionRepository = mock[SessionRepository]

      when(mockSessionRepository.set(any())) thenReturn Future.successful(true)

      val application =
        applicationBuilder(userAnswers = Some(emptyUserAnswers))
          .overrides(
            bind[Navigator].toInstance(new FakeNavigator(onwardRoute)),
            bind[SessionRepository].toInstance(mockSessionRepository)
          )
          .build()

      running(application) {

        val result = route(application, postRequest).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual onwardRoute.url
      }
    }

    "return a Bad Request and errors when invalid data is submitted" in {

      val application = applicationBuilder(userAnswers = Some(emptyUserAnswers)).build()

      running(application) {

        val request =
          fakeRequest(POST, $className;format="decap"$Route)
            .withFormUrlEncodedBody(("value", "invalid value"))

        val boundForm = form.bind(Map("value" -> "invalid value"))

        val view = application.injector.instanceOf[$className$View]

        val result = route(application, request).value

        status(result) mustEqual BAD_REQUEST

        contentAsString(result) mustEqual
          view(boundForm, NormalMode)(request, messages(application)).toString
      }
    }

    "redirect to Session Expired for a GET if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val result = route(application, getRequest).value

        status(result) mustEqual SEE_OTHER
        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }

    "redirect to Session Expired for a POST if no existing data is found" in {

      val application = applicationBuilder(userAnswers = None).build()

      running(application) {

        val result = route(application, postRequest).value

        status(result) mustEqual SEE_OTHER

        redirectLocation(result).value mustEqual routes.SessionExpiredController.onPageLoad().url
      }
    }
  }
}
