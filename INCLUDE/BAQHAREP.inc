 /*********************************************************************
  * Copyright IBM Corp. 2025
  *
  * Licensed under the Apache License, Version 2.0 (the "License");
  * you may not use this file except in compliance with the License.
  * You may obtain a copy of the License at
  *
  *     http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing
  * , software distributed under the License is distributed on an
  * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
  * either express or implied. See the License for the specific
  * language governing permissions and limitations under the
  * License.
  *********************************************************************/

  /*********************************************************************
  * This file contains the language structures required by PL/I        *
  * programs to work with the API requester Host API.                  *
  *********************************************************************/
 DCL 01 BAQ_ZCONNECT_AREA,
       03 BAQ_ZCON_AREA_EYE           CHAR(4) INIT('BAQZ'),
       03 BAQ_ZCON_AREA_LENGTH        UNSIGNED BIN FIXED(32) INIT(4648),
       03 BAQ_ZCON_AREA_VERSION       UNSIGNED BIN FIXED(32) INIT(1),
       03 BAQ_ZCON_RESERVED_01        UNSIGNED BIN FIXED(32) INIT(1),
       03 BAQ_ZCON_PARAMETERS         UNION,
        05 BAQ_ZCON_PARM_INIT,
         07 BAQ_ZCON_RESERVED_02      CHAR(3584) INIT((3584)'00'x),
        05 BAQ_ZCON_PARMS (64),
         07 BAQ_ZCON_PARM_NAME        CHAR(48),
         07 BAQ_ZCON_PARM_ADDRESS     POINTER,
         07 BAQ_ZCON_PARM_LENGTH      UNSIGNED BIN FIXED(32),
       03 BAQ_ZCON_RETURN_CODES,
        05 BAQ_ZCON_COMPLETION_CODE   UNSIGNED BIN FIXED(32) INIT(0),
        05 BAQ_ZCON_REASON_CODE       UNSIGNED BIN FIXED(32) INIT(0),
        05 BAQ_ZCON_SERVICE_ID        UNSIGNED BIN FIXED(32) INIT(0),
        05 BAQ_ZCON_SERVICE_CODE      UNSIGNED BIN FIXED(32) INIT(0),
        05 BAQ_ZCON_RESERVED_03       UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_ZCON_RETURN_MESSAGE_LEN UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_ZCON_RETURN_MESSAGE     CHAR(1024);

 DCL 01 BAQ_REQUEST_AREA,
       03 BAQ_REQ_AREA_EYE            CHAR(4) INIT('BAQR'),
       03 BAQ_REQ_AREA_LENGTH         UNSIGNED BIN FIXED(32) INIT(3608),
       03 BAQ_REQ_AREA_VERSION        UNSIGNED BIN FIXED(32) INIT(1),
       03 BAQ_REQ_RESERVED_01         UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_REQ_BASE_ADDRESS        POINTER,
       03 BAQ_REQ_BASE_LENGTH         UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_REQ_PARAMETERS          UNION,
        05 BAQ_REQ_PARM_INIT,
         07 BAQ_REQ_RESERVED_03       CHAR(3584) INIT((3584)'00'x),
        05 BAQ_REQ_PARMS (64),
         07 BAQ_REQ_PARM_NAME         CHAR(48),
         07 BAQ_REQ_PARM_ADDRESS      POINTER,
         07 BAQ_REQ_PARM_LENGTH       UNSIGNED BIN FIXED(32);

 DCL 01 BAQ_RESPONSE_AREA,
       03 BAQ_RESP_AREA_EYE           CHAR(4) INIT('BAQP'),
       03 BAQ_RESP_AREA_LENGTH        UNSIGNED BIN FIXED(32) INIT(1060),
       03 BAQ_RESP_AREA_VERSION       UNSIGNED BIN FIXED(32) INIT(1),
       03 BAQ_RESP_RESERVED_01        UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_RESP_BASE_ADDRESS       POINTER,
       03 BAQ_RESP_BASE_LENGTH        UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_RESP_RESERVED_02        UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_RESP_STATUS_CODE        UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_RESP_STATUS_MESSAGE_LEN UNSIGNED BIN FIXED(32) INIT(0),
       03 BAQ_RESP_STATUS_MESSAGE     CHAR(1024);

