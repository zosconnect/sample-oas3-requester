 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 | This file contains the generated API information structure
 | which is passed to the Host API via the BAQEXEC call.
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
     Dcl 1 BAQ_API_INFO_RBK01I01,
        3 BAQ_API_INFO_EYE            CHAR(4) INIT('BAQA'),
        3 BAQ_API_INFO_LENGTH         FIXED BIN(32) ALIGNED
          INIT(1052) UNSIGNED,
        3 BAQ_API_INFO_VERSION        FIXED BIN(32) ALIGNED
          INIT(1) UNSIGNED,
        3 BAQ_API_INFO_RESERVED01     FIXED BIN(32) ALIGNED
          INIT(0) UNSIGNED,
        3 BAQ_API_NAME                CHAR(255)
          INIT('RedbookApi'),
        3 BAQ_API_NAME_LEN            FIXED BIN(32) ALIGNED
          INIT(10) UNSIGNED,
        3 BAQ_API_PATH                CHAR(255)
          INIT('%2Fredbook%2F%7Btitle%7D'),
        3 BAQ_API_PATH_LEN            FIXED BIN(32) ALIGNED
          INIT(24) UNSIGNED,
        3 BAQ_API_METHOD              CHAR(255)
          INIT('POST'),
        3 BAQ_API_METHOD_LEN          FIXED BIN(32) ALIGNED
          INIT(4) UNSIGNED,
        3 BAQ_API_OPERATION           CHAR(255)
          INIT('createRedbook'),
        3 BAQ_API_OPERATION_LEN       FIXED BIN(32) ALIGNED
          INIT(13) UNSIGNED;
