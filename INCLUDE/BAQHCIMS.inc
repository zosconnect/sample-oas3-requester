 /*********************************************************************
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
  *********************************************************************/

 /**********************************************************************
  * BAQHCIMS                                                           *
  *                                                                    *
  * This file contains the constants used by the IMS PL/I sample       *
  * programs.                                                          *
  *********************************************************************/

 /**
  * DL/I Functions
  */
    DCL AIBTDLI ENTRY OPTIONS(ASM);
    DCL PLITDLI ENTRY EXTERNAL;
    FETCH AIBTDLI;
    FETCH PLITDLI;

 /**
  * DL/I Function Codes
  */
    DCL DLI_INQY                       CHAR(4) VALUE('INQY');
    DCL DLI_GET_UNIQUE                 CHAR(4) VALUE('GU  ');
    DCL DLI_GET_NEXT                   CHAR(4) VALUE('GN  ');
    DCL DLI_INSERT                     CHAR(4) VALUE('ISRT');

 /**
  * DL/I Call Status Codes
  */    
    DCL DLI_MESSAGE_EXISTS             CHAR(2) VALUE('CF');
    DCL DLI_END_SEGMENTS               CHAR(2) VALUE('QD');
    DCL DLI_END_MESSAGES               CHAR(2) VALUE('QC');
    DCL DLI_STATUS_OK                  CHAR(2) VALUE('  ');

 /**
  * INQY Data Structures
  */    
    DCL 01 INQY_IO_AREA,
            05 INQY_ENVIRON_DATA       CHAR(108) INIT((108)'40'x),
            05 INQY_LEN_RECOVERY_SEC   BIN FIXED(15) INIT(16),
            05 INQY_RECOVERY_SECTION   CHAR(16) INIT((16)'40'x),
            05 INQY_LEN_APARM          BIN FIXED(15) INIT(32),
            05 INQY_APARM              CHAR(32) INIT((32)'40'x);

    DCL 01 INQY_ENVIRON UNION,
          03 INQY_ENVIRON_INIT,
            05 INQY_INIT_SPACES        CHAR(104) INIT((104)'40'x),
          03 INQY_ENVIRON_FIELDS,
            05 INQY_IMS_ID             CHAR(08),
            05 INQY_IMS_RELEASE        CHAR(04),
            05 INQY_IMS_CTRL_REG_TYPE  CHAR(08),
            05 INQY_IMS_APPL_REG_TYPE  CHAR(08),
            05 INQY_IMS_REGION_ID      CHAR(04),
            05 INQY_APPL_PGM_NAME      CHAR(08),
            05 INQY_PSB_NAME           CHAR(08),
            05 INQY_TRANS_NAME         CHAR(08),
            05 INQY_USER_ID            CHAR(08),
            05 INQY_GROUP_NAME         CHAR(08),
            05 INQY_STATUS_GRP_IND     CHAR(04),
            05 INQY_ADDR_REC_TOKEN     CHAR(04),
            05 INQY_ADDR_APPL_PARM     CHAR(04),
            05 INQY_SHR_QUEUE_IND      CHAR(04),
            05 INQY_USERID_ADDR_SP     CHAR(08),
            05 INQY_USERID_IND         CHAR(01),
            05 INQY_RRS_IND            CHAR(03),
            05 INQY_CATALOG_IND        CHAR(08);
 /**
  * PCB Data Structure
  */
    DCL 01 IO_PCB_MASK_PTR             POINTER;
    DCL 01 IO_PCB_MASK                 BASED(IO_PCB_MASK_PTR),
           05 IO_PCB_LTERM             CHAR(8),
           05 RESV                     CHAR(2),
           05 IO_PCB_STATUS_CODE       CHAR(2),
           05 IO_PCB_DATE              CHAR(4),
           05 IO_PCB_TIME              CHAR(4),
           05 IO_PCB_MSG_SEG_NUMBER    CHAR(4),
           05 IO_PCB_MOD_NAME          CHAR(8),
           05 IO_PCB_USER_ID           CHAR(8);
 /**
  * AIB Data Structure
  */
    DCL 01 AIB_CONTROL,
            05 AIB_ID                  CHAR(08) INIT('DFSAIB  '),
            05 AIB_LEN                 UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_SUB_FUNC            CHAR(08) INIT('ENVIRON '),
            05 AIB_RSRC_NAME1          CHAR(08) INIT('IOPCB   '),
            05 AIB_RSRC_NAME2          CHAR(08) INIT((8)'40'x),
            05 AIB_RESERVED1           CHAR(08) INIT((8)'00'x),
            05 AIB_OUT_AREA_LEN        UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_OUT_AREA_USE        UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_RSRC_FIELD          UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_OP_AREA_LEN         UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_FLAG1               CHAR(01) INIT('00'x),
            05 AIB_RESERVED2           CHAR(03) INIT((3)'00'x),
            05 AIB_RETURN_CODE         UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_REASON_CODE         UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_ERR_CODE_EXT        UNSIGNED BIN FIXED(32) INIT(0),
            05 AIB_RSRC_ADDR1          POINTER INIT(SYSNULL()),
            05 AIB_RSRC_ADDR2          POINTER INIT(SYSNULL()),
            05 AIB_RSRC_ADDR3          POINTER INIT(SYSNULL()),
            05 AIB_USER_TOKEN          CHAR(16) INIT((16)'00'x),
            05 AIB_RESERVED5           CHAR(4) INIT((4)'00'x),
            05 AIB_UNUSED_SAVE         CHAR(16) INIT((16)'00'x),
            05 AIB_UNUSED_TOKN         CHAR(6) INIT((6)'00'x),
            05 AIB_UNUSED_TOKC         CHAR(16) INIT((16)'00'x),
            05 AIB_UNUSED_TOKV         CHAR(16) INIT((16)'00'x),
            05 AIB_UNUSED_TOKA         CHAR(8) INIT((8)'00'x),
            05 AIB_UNUSED_TOKA_EYE     CHAR(8) INIT((8)'00'x),
            05 AIB_RESERVED6           CHAR(4) INIT((4)'00'x);

