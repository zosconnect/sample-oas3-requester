 /*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 | This file contains the generated API information structure
 | which is passed to the Host API via the BAQEXEC call.
 +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
     Dcl 1 BAQ_API_INFO_RBK04I01,
        3 BAQ_API_INFO_EYE            CHAR(4) INIT('BAQA'),
        3 BAQ_API_INFO_LENGTH         FIXED BIN(32) ALIGNED
          INIT(1311) UNSIGNED,
        3 BAQ_API_INFO_VERSION        FIXED BIN(32) ALIGNED
          INIT(2) UNSIGNED,
        3 BAQ_API_INFO_RESERVED01     FIXED BIN(32) ALIGNED
          INIT(0) UNSIGNED,
        3 BAQ_API_NAME                CHAR(255)
          INIT('RedbookApiPLI'),
        3 BAQ_API_NAME_LEN            FIXED BIN(32) ALIGNED
          INIT(13) UNSIGNED,
        3 BAQ_API_PATH                CHAR(255)
          INIT('%2Fredbooks'),
        3 BAQ_API_PATH_LEN            FIXED BIN(32) ALIGNED
          INIT(11) UNSIGNED,
        3 BAQ_API_METHOD              CHAR(255)
          INIT('GET'),
        3 BAQ_API_METHOD_LEN          FIXED BIN(32) ALIGNED
          INIT(3) UNSIGNED,
        3 BAQ_API_OPERATION           CHAR(255)
          INIT('getAllRedbooks'),
        3 BAQ_API_OPERATION_LEN       FIXED BIN(32) ALIGNED
          INIT(14) UNSIGNED,
        3 BAQ_API_MEDIA_TYPE          CHAR(255)
          INIT(''),
        3 BAQ_API_MEDIA_TYPE_LEN      FIXED BIN(32) ALIGNED
          INIT(0) UNSIGNED;
