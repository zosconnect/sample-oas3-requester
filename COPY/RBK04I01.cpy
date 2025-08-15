      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated API information structure
      * which is passed to the Host API via the BAQEXEC call.
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
         01 BAQ-API-INFO-RBK04I01.
           03 BAQ-API-INFO-EYE            PIC X(4)
              VALUE 'BAQA'.
           03 BAQ-API-INFO-LENGTH         PIC 9(9) COMP-5 SYNC
              VALUE 1311.
           03 BAQ-API-INFO-VERSION        PIC 9(9) COMP-5 SYNC
              VALUE 2.
           03 BAQ-API-INFO-RESERVED01     PIC 9(9) COMP-5 SYNC
              VALUE 0.
           03 BAQ-API-NAME                PIC X(255)
              VALUE 'RedbookApi'.
           03 BAQ-API-NAME-LEN            PIC 9(9) COMP-5 SYNC
              VALUE 10.
           03 BAQ-API-PATH                PIC X(255)
              VALUE '%2Fredbooks'.
           03 BAQ-API-PATH-LEN            PIC 9(9) COMP-5 SYNC
              VALUE 11.
           03 BAQ-API-METHOD              PIC X(255)
              VALUE 'GET'.
           03 BAQ-API-METHOD-LEN          PIC 9(9) COMP-5 SYNC
              VALUE 3.
           03 BAQ-API-OPERATION           PIC X(255)
              VALUE 'getAllRedbooks'.
           03 BAQ-API-OPERATION-LEN       PIC 9(9) COMP-5 SYNC
              VALUE 14.
           03 BAQ-API-MEDIA-TYPE          PIC X(255)
              VALUE ' '.
           03 BAQ-API-MEDIA-TYPE-LEN      PIC 9(9) COMP-5 SYNC
              VALUE 0.
