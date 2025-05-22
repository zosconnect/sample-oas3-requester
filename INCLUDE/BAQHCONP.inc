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
 DCL BAQINIT EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);
 DCL BAQEXEC EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);
 DCL BAQGETN EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);
 DCL BAQPUTN EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);
 DCL BAQFREE EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);
 DCL BAQTERM EXTERNAL ENTRY OPTIONS(NODESCRIPTOR)
             RETURNS(FIXED BIN(31) BYVALUE);

 FETCH BAQINIT;
 FETCH BAQEXEC;
 FETCH BAQGETN;
 FETCH BAQPUTN;
 FETCH BAQFREE;
 FETCH BAQTERM;

 /*
  * Host API Completion Codes
  */
 DCL 01 BAQ_SUCCESS  FIXED BIN(31) INIT(0);
 DCL 01 BAQ_WARNING  FIXED BIN(31) INIT(4);
 DCL 01 BAQ_ERROR    FIXED BIN(31) INIT(8);
 DCL 01 BAQ_SEVERE   FIXED BIN(31) INIT(12);
 DCL 01 BAQ_CRITICAL FIXED BIN(31) INIT(16);

 /*
  * Host API Request parameter names
  */
 DCL 01 BAQR_OAUTH_USERNAME      CHAR(22)
           INIT('BAQHAPI-oAuth-Username');
 DCL 01 BAQR_OAUTH_PASSWORD      CHAR(22)
           INIT('BAQHAPI-oAuth-Password');
 DCL 01 BAQR_OAUTH_SCOPE         CHAR(19)
           INIT('BAQHAPI-oAuth-Scope');
 DCL 01 BAQR_OAUTH_CLIENT_ID     CHAR(22)
           INIT('BAQHAPI-oAuth-ClientId');
 DCL 01 BAQR_OAUTH_CLIENT_SECRET CHAR(26)
           INIT('BAQHAPI-oAuth-ClientSecret');
 DCL 01 BAQR_OAUTH_RESOURCE      CHAR(22)
           INIT('BAQHAPI-oAuth-Resource');
 DCL 01 BAQR_OAUTH_AUDIENCE      CHAR(22)
           INIT('BAQHAPI-oAuth-Audience');
 DCL 01 BAQR_OAUTH_CUSTOM_PARMS  CHAR(25)
           INIT('BAQHAPI-oAuth-CustomParms');
 DCL 01 BAQR_TOKEN_USERNAME      CHAR(22)
           INIT('BAQHAPI-Token-Username');
 DCL 01 BAQR_TOKEN_PASSWORD      CHAR(22)
           INIT('BAQHAPI-Token-Password');
 DCL 01 BAQR_TOKEN_CUSTOM_PARMS  CHAR(25)
           INIT('BAQHAPI-Token-CustomParms');
 DCL 01 BAQR_TOKEN_CUSTOM_HEADERS  CHAR(27)
           INIT('BAQHAPI-Token-CustomHeaders');
 DCL 01 BAQR_POLICY_SELECTION  CHAR(24)
           INIT('BAQHAPI-Policy-Selection');

 /*
  * Host API ZCON parameter names
  */
 DCL 01 BAQZ_TRACE_VERBOSE      CHAR(21)
           INIT('BAQHAPI-Trace-Verbose');
 DCL 01 BAQZ_SERVER_URIMAP      CHAR(21)
           INIT('BAQHAPI-Server-URIMAP');
 DCL 01 BAQZ_SERVER_HOST        CHAR(19)
           INIT('BAQHAPI-Server-Host');
 DCL 01 BAQZ_SERVER_PORT        CHAR(19)
           INIT('BAQHAPI-Server-Port');
 DCL 01 BAQZ_SERVER_TIMEOUT     CHAR(22)
           INIT('BAQHAPI-Server-Timeout');
 DCL 01 BAQZ_SERVER_USERNAME    CHAR(23)
           INIT('BAQHAPI-Server-Username');
 DCL 01 BAQZ_SERVER_PASSWORD    CHAR(23)
           INIT('BAQHAPI-Server-Password');

 /*
  * Host API TRACE VERBOSE levels
  */
 DCL 01 BAQZ_TRACE_LEVEL_OFF    CHAR(3)
           INIT('OFF');
 DCL 01 BAQZ_TRACE_LEVEL_ON     CHAR(2)
           INIT('ON');
 DCL 01 BAQZ_TRACE_LEVEL_ERROR  CHAR(5)
           INIT('ERROR');
 DCL 01 BAQZ_TRACE_LEVEL_AUDIT  CHAR(5)
           INIT('AUDIT');
 DCL 01 BAQZ_TRACE_LEVEL_ALL    CHAR(3)
           INIT('ALL');

 /*
  * Deprecated Host API Request parameter names
  */
 DCL 01 BAQR_JWT_USERNAME        CHAR(22)
           INIT('BAQHAPI-Token-Username');
 DCL 01 BAQR_JWT_PASSWORD        CHAR(22)
           INIT('BAQHAPI-Token-Password');