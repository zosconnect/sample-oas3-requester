      *****************************************************************
      * Copyright IBM Corp. 2024
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
      * BAQHCIMS                                                      *
      *                                                               *
      * This file contains the constants used by the IMS COBOL sample *
      * programs.                                                     *
      *****************************************************************
      
      *
      * DL/I CONTSTANTS
      *   
         77 CBLTDLI           PIC X(8) VALUE 'CBLTDLI'.
      
      *
      * DL/I FUNCTION CODES
      *
         77  DLI-GET-UNIQUE       PIC X(4)  VALUE 'GU  '.
         77  DLI-GET-NEXT         PIC X(4)  VALUE 'GN  '.

      *   
      * DL/I CALL STATUS CODES
      *
         77  DLI-STATUS-OK        PIC X(2) VALUE '  '.
         77  DLI-MESSAGE-EXIST    PIC X(2) VALUE 'CF'.
         77  DLI-NO-MORE-SEGMENT  PIC X(2) VALUE 'QD'.
         77  DLI-NO-MORE-MESSAGE  PIC X(2) VALUE 'QC'.

      *
      * INQY CONSTANTS
      *   
         77 INQY              PIC X(4) VALUE 'INQY'.
         77 AIBTDLI           PIC X(8) VALUE 'AIBTDLI'.

      *
      * INQY DATA STRUCTURES
      * 
         01 INQY-IO-AREA.
           05 INQY-ENVIRON-DATA       PIC X(108) VALUE SPACES.
           05 INQY-LEN-RECOVERY-SEC   PIC S9(04) VALUE 16 COMP.
           05 INQY-RECOVERY-SECTION   PIC X(016) VALUE SPACES.
           05 INQY-LEN-APARM          PIC S9(04) VALUE 32 COMP.
           05 INQY-APARM              PIC X(032) VALUE SPACES.

         01 INQY-ENVIRON.
           05 INQY-IMS-ID             PIC X(08) VALUE SPACES.
           05 INQY-IMS-RELEASE        PIC X(04) VALUE SPACES.
           05 INQY-IMS-CTRL-REG-TYPE  PIC X(08) VALUE SPACES.
           05 INQY-IMS-APPL-REG-TYPE  PIC X(08) VALUE SPACES.
           05 INQY-IMS-REGION-ID      PIC X(04) VALUE SPACES.
           05 INQY-APPL-PGM-NAME      PIC X(08) VALUE SPACES.
           05 INQY-PSB-NAME           PIC X(08) VALUE SPACES.
           05 INQY-TRANS-NAME         PIC X(08) VALUE SPACES.
           05 INQY-USER-ID            PIC X(08) VALUE SPACES.
           05 INQY-GROUP-NAME         PIC X(08) VALUE SPACES.
           05 INQY-STATUS-GRP-IND     PIC X(04) VALUE SPACES.
           05 INQY-ADDR-REC-TOKEN     PIC X(04) VALUE SPACES.
           05 INQY-ADDR-APPL-PARM     PIC X(04) VALUE SPACES.
           05 INQY-SHR-QUEUE-IND      PIC X(04) VALUE SPACES.
           05 INQY-USERID-ADDR-SP     PIC X(08) VALUE SPACES.
           05 INQY-USERID-IND         PIC X(01) VALUE SPACES.
           05 INQY-RRS-IND            PIC X(03) VALUE SPACES.
           05 INQY-CATALOG-IND        PIC X(08) VALUE SPACES.

      *
      * AIB DATA STRUCTURE
      * 
         01 AIB-CONTROL.
           05 AIB-ID                   PIC X(08) VALUE 'DFSAIB  '.
           05 AIB-LEN                  PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-SUB-FUNC             PIC X(08) VALUE 'ENVIRON '.
           05 AIB-RSRC-NAME1           PIC X(08) VALUE 'IOPCB   '.             
           05 AIB-RSRC-NAME2           PIC X(08) VALUE SPACES.
           05 AIB-RESERVED1            PIC X(08) VALUE LOW-VALUE.
           05 AIB-OUT-AREA-LEN         PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-OUT-AREA-USE         PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-RSRC-FIELD           PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-OP-AREA-LEN          PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-FLAG1                PIC X(01) VALUE LOW-VALUE.
           05 AIB-RESERVED2            PIC X(03) VALUE LOW-VALUE.
           05 AIB-RETURN-CODE          PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-REASON-CODE          PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-ERR-CODE-EXT         PIC 9(09) VALUE 0 USAGE COMP-5.
           05 AIB-RSRC-ADDR1           USAGE POINTER VALUE NULL.
           05 AIB-RSRC-ADDR2           USAGE POINTER VALUE NULL.
           05 AIB-RSRC-ADDR3           USAGE POINTER VALUE NULL.
           05 AIB-USER-TOKEN           PIC X(16) VALUE LOW-VALUE.
           05 AIB-RESERVED5            PIC X(4) VALUE LOW-VALUE.
           05 AIB-UNUSED-SAVE          PIC X(16) VALUE LOW-VALUE.
           05 AIB-UNUSED-TOKN          PIC X(6) VALUE LOW-VALUE.
           05 AIB-UNUSED-TOKC          PIC X(16) VALUE LOW-VALUE.
           05 AIB-UNUSED-TOKV          PIC X(16) VALUE LOW-VALUE.
           05 AIB-UNUSED-TOKA          PIC X(8) VALUE LOW-VALUE.
           05 AIB-UNUSED-TOKA-EYE      PIC X(8) VALUE LOW-VALUE.
           05 AIB-RESERVED6            PIC X(4) VALUE LOW-VALUE.