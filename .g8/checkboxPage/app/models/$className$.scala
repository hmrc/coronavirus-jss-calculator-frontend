package models

import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.checkboxes.CheckboxItem

sealed trait $className$

object $className$ extends Enumerable.Implicits {

  case object $option1key;format="Camel"$ extends WithName("$option1key;format="decap"$") with $className$
  case object $option2key;format="Camel"$ extends WithName("$option2key;format="decap"$") with $className$

  val values: Seq[$className$] = Seq(
    $option1key;format="Camel"$,
    $option2key;format="Camel"$
  )

  def options(form: Form[_])(implicit messages: Messages): Seq[CheckboxItem] = values.map {
    value =>
      CheckboxItem(
        name = Some("value[]"),
        id = Some(value.toString),
        value = value.toString,
        content = Text(messages(s"$className;format="decap"$.\${value.toString}")),
        checked = form.data.exists(_._2 == value.toString)
    )
  }

  implicit val enumerable: Enumerable[$className$] =
    Enumerable(values.map(v => v.toString -> v): _*)
}
