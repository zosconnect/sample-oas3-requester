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
      * This file contains the language structures required by COBOL
      * programs to work with the API requester Host API.
      *****************************************************************
       01 BAQ-ZCONNECT-AREA.
         03 BAQ-ZCON-AREA-EYE            PIC X(4) VALUE 'BAQZ'.
         03 BAQ-ZCON-AREA-LENGTH         PIC 9(8) COMP-5 VALUE 4648.
         03 BAQ-ZCON-AREA-VERSION        PIC 9(8) COMP-5 VALUE 1.
         03 BAQ-ZCON-RESERVED-01         PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-ZCON-PARM-INIT           VALUE LOW-VALUES.
          05 BAQ-ZCON-RESERVED-02        PIC X(3584).
         03 BAQ-ZCON-PARAMETERS          REDEFINES BAQ-ZCON-PARM-INIT.
          05 BAQ-ZCON-PARMS              OCCURS 64.
           07 BAQ-ZCON-PARM-NAME         PIC X(48).
           07 BAQ-ZCON-PARM-ADDRESS      USAGE POINTER.
           07 BAQ-ZCON-PARM-LENGTH       PIC 9(9) BINARY.
         03 BAQ-ZCON-RETURN-CODES.
          05 BAQ-ZCON-COMPLETION-CODE    PIC 9(8) COMP-5 VALUE 0.
           88 BAQ-SUCCESS                VALUE 0.
           88 BAQ-WARNING                VALUE 4.
           88 BAQ-ERROR                  VALUE 8.
           88 BAQ-SEVERE                 VALUE 12.
           88 BAQ-CRITICAL               VALUE 16.
          05 BAQ-ZCON-REASON-CODE        PIC 9(8) COMP-5 VALUE 0.
          05 BAQ-ZCON-SERVICE-ID         PIC 9(8) COMP-5 VALUE 0.
          05 BAQ-ZCON-SERVICE-CODE       PIC 9(8) COMP-5 VALUE 0.
          05 BAQ-ZCON-RESERVED-03        PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-ZCON-RETURN-MESSAGE-LEN  PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-ZCON-RETURN-MESSAGE      PIC X(1024).

       01 BAQ-REQUEST-AREA.
         03 BAQ-REQ-AREA-EYE             PIC X(4)  VALUE 'BAQR'.
         03 BAQ-REQ-AREA-LENGTH          PIC 9(8) COMP-5 VALUE 3608.
         03 BAQ-REQ-AREA-VERSION         PIC 9(8) COMP-5 VALUE 1.
         03 BAQ-REQ-RESERVED-01          PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-REQ-BASE-ADDRESS         USAGE POINTER.
         03 BAQ-REQ-BASE-LENGTH          PIC 9(8) BINARY.
         03 BAQ-REQ-PARM-INIT            VALUE LOW-VALUES.
          05 BAQ-REQ-RESERVED-02         PIC X(3584).
         03 BAQ-REQ-PARAMETERS           REDEFINES BAQ-REQ-PARM-INIT.
          05 BAQ-REQ-PARMS               OCCURS 64.
           07 BAQ-REQ-PARM-NAME          PIC X(48).
           07 BAQ-REQ-PARM-ADDRESS       USAGE POINTER.
           07 BAQ-REQ-PARM-LENGTH        PIC 9(9) BINARY.

       01 BAQ-RESPONSE-AREA.
         03 BAQ-RESP-AREA-EYE            PIC X(4)  VALUE 'BAQP'.
         03 BAQ-RESP-AREA-LENGTH         PIC 9(8) COMP-5 VALUE 1060.
         03 BAQ-RESP-AREA-VERSION        PIC 9(8) COMP-5 VALUE 1.
         03 BAQ-RESP-RESERVED-01         PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-RESP-BASE-ADDRESS        USAGE POINTER.
         03 BAQ-RESP-BASE-LENGTH         PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-RESP-RESERVED-02         PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-RESP-STATUS-CODE         PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-RESP-STATUS-MESSAGE-LEN  PIC 9(8) COMP-5 VALUE 0.
         03 BAQ-RESP-STATUS-MESSAGE      PIC X(1024).