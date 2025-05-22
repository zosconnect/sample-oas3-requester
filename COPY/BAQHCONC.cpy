      *****************************************************************
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
      *****************************************************************
      
      *****************************************************************
      * This file contains the constants required by COBOL programs to
      * work with the API requester Host API.
      *****************************************************************

      * Host API entry point names
       77 BAQ-INIT-NAME          PIC X(8) VALUE 'BAQINIT'.
       77 BAQ-EXEC-NAME          PIC X(8) VALUE 'BAQEXEC'.
       77 BAQ-GETN-NAME          PIC X(8) VALUE 'BAQGETN'.
       77 BAQ-PUTN-NAME          PIC X(8) VALUE 'BAQPUTN'.
       77 BAQ-FREE-NAME          PIC X(8) VALUE 'BAQFREE'.
       77 BAQ-TERM-NAME          PIC X(8) VALUE 'BAQTERM'.

      * Host API Request parameter names
       77 BAQR-OAUTH-USERNAME      PIC X(22)
           VALUE 'BAQHAPI-oAuth-Username'.
       77 BAQR-OAUTH-PASSWORD      PIC X(22)
           VALUE 'BAQHAPI-oAuth-Password'.
       77 BAQR-OAUTH-SCOPE         PIC X(19)
           VALUE 'BAQHAPI-oAuth-Scope'.
       77 BAQR-OAUTH-CLIENT-ID     PIC X(22)
           VALUE 'BAQHAPI-oAuth-ClientId'.
       77 BAQR-OAUTH-CLIENT-SECRET PIC X(26)
           VALUE 'BAQHAPI-oAuth-ClientSecret'.
       77 BAQR-OAUTH-RESOURCE      PIC X(22)
           VALUE 'BAQHAPI-oAuth-Resource'.
       77 BAQR-OAUTH-AUDIENCE      PIC X(22)
           VALUE 'BAQHAPI-oAuth-Audience'.
       77 BAQR-OAUTH-CUSTOM-PARMS  PIC X(25)
           VALUE 'BAQHAPI-oAuth-CustomParms'.
       77 BAQR-TOKEN-USERNAME      PIC X(22)
           VALUE 'BAQHAPI-Token-Username'.
       77 BAQR-TOKEN-PASSWORD      PIC X(22)
           VALUE 'BAQHAPI-Token-Password'.
       77 BAQR-TOKEN-CUSTOM-PARMS  PIC X(25)
           VALUE 'BAQHAPI-Token-CustomParms'.
       77 BAQR-TOKEN-CUSTOM-HEADERS  PIC X(27)
           VALUE 'BAQHAPI-Token-CustomHeaders'.
       77 BAQR-POLICY-SELECTION  PIC X(24)
           VALUE 'BAQHAPI-Policy-Selection'.

      * Host API ZCON parameter names
       77 BAQZ-TRACE-VERBOSE      PIC X(21)
           VALUE 'BAQHAPI-Trace-Verbose'.
       77 BAQZ-SERVER-URIMAP      PIC X(21)
           VALUE 'BAQHAPI-Server-URIMAP'.
       77 BAQZ-SERVER-HOST        PIC X(19)
           VALUE 'BAQHAPI-Server-Host'.
       77 BAQZ-SERVER-PORT        PIC X(19)
           VALUE 'BAQHAPI-Server-Port'.
       77 BAQZ-SERVER-TIMEOUT     PIC X(22)
           VALUE 'BAQHAPI-Server-Timeout'.
       77 BAQZ-SERVER-USERNAME    PIC X(23)
           VALUE 'BAQHAPI-Server-Username'.
       77 BAQZ-SERVER-PASSWORD    PIC X(23)
           VALUE 'BAQHAPI-Server-Password'.

      * Host API TRACE VERBOSE levels
       77 BAQZ-TRACE-LEVEL-OFF    PIC X(3)
           VALUE 'OFF'.
       77 BAQZ-TRACE-LEVEL-ON     PIC X(2)
           VALUE 'ON'.
       77 BAQZ-TRACE-LEVEL-ERROR  PIC X(5)
           VALUE 'ERROR'.
       77 BAQZ-TRACE-LEVEL-AUDIT  PIC X(5)
           VALUE 'AUDIT'.
       77 BAQZ-TRACE-LEVEL-ALL    PIC X(3)
           VALUE 'ALL'.

      * Deprecated Host API Request parameter names
       77 BAQR-JWT-USERNAME        PIC X(22)
           VALUE 'BAQHAPI-Token-Username'.
       77 BAQR-JWT-PASSWORD        PIC X(22)
           VALUE 'BAQHAPI-Token-Password'.