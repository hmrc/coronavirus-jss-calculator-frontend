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

import services.NuanceCryptoService
import uk.gov.hmrc.http.HeaderCarrier

case class EncryptedNuanceData private (nuanceSessionId: String, mdtpSessionID: String, deviceID: String)

object EncryptedNuanceData {

  /** Construct encrypted fields using data from request and header carrier
    */
  def create(cryptoService: NuanceCryptoService, hc: HeaderCarrier): EncryptedNuanceData =
    EncryptedNuanceData(
      cryptoService.nuanceSafeHash(sessionId(hc)),
      cryptoService.encryptField(sessionId(hc)),
      cryptoService.encryptField(deviceID(hc))
    )

  private def sessionId(hc: HeaderCarrier): String = hc.sessionId.fold("")(_.value)

  private def deviceID(hc: HeaderCarrier): String = hc.deviceID.getOrElse("")
}
