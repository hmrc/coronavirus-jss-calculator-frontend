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

package viewmodels

import play.api.i18n.Messages
import play.twirl.api.Html
import uk.gov.hmrc.govukfrontend.views.viewmodels.content.{HtmlContent, Text}
import uk.gov.hmrc.govukfrontend.views.viewmodels.summarylist.{Actions, Key, SummaryListRow, Value}

trait SummaryListRowHelper {

  def summaryListRow(label: String, value: String, actions: Actions): SummaryListRow =
    SummaryListRow(
      key = Key(
        content = Text(label)
      ),
      value = Value(
        content = HtmlContent(value)
      ),
      actions = Some(actions)
    )

  protected def yesOrNo(value: Boolean)(implicit messages: Messages): String =
    if (value) messages("site.yes") else messages("site.no")

  def span(contents: String): HtmlContent = HtmlContent(
    Html(s"""<span aria-hidden="true">$contents</span>""")
  )
}
