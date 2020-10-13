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

package formats

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import play.api.i18n.Messages

object Format {

  def dateFormatter(implicit messages: Messages): DateTimeFormatter = DateTimeFormatter.ofPattern("d MMMM yyyy", messages.lang.toLocale)
  def formattedDate(date: LocalDate)(implicit messages: Messages): String = date.format(dateFormatter)
  def formattedDate(date: LocalDateTime)(implicit messages: Messages): String = date.format(dateFormatter)
}
