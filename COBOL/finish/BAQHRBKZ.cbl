      *****************************************************************
      * Copyright IBM Corp. 2023, 2025
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
      * BAQHRBKZ                                                      *
      *                                                               *
      * Calls RedbookAPI endpoint application operations.             *
      *                                                               *
      * Operation Parameters:                                         *
      *      GARB - Get All Redbooks                                  *
      *      GRBK - Get Redbook                                       *
      *      CRBK - Create Redbook                                    *
      *      PRBK - Patch Redbook                                     *
      *      MRBK - Merge Redbook                                     *
      *                                                               *
      * Pass in via JCL PARM statement, E.g:                          *
      *    //RBKRUN EXEC PGM=BAQHRBKZ,PARM='GARB'                     *
      *                                                               *
      * Optionally specify DEBUG for more diagnostics, E.g:           *
      *    //RBKRUN EXEC PGM=BAQHRBKZ,PARM='GARB DEBUG'               *
      *                                                               *
      * Calls RedbookAPI endpoint application operations.             *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAQHRBKZ.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * API requester Host API required copy books
       COPY BAQHAREC.
       COPY BAQHCONC.

      * API-INFO for Operation getRedbook
       COPY RBK00I01.

      * API-INFO for Operation createRedbook
       COPY RBK01I01.

      * API-INFO for Operation patchRedbook
       COPY RBK02I01.

      * API-INFO for Operation mergeRedbook
       COPY RBK03I01.

      * API-INFO for Operation getAllRedbooks
       COPY RBK04I01.

      * Pointer to API-INFO structure
       01 WS-API-INFO        USAGE POINTER VALUE NULL.

      * Data Area name to get
       01 WS-DATA-AREA-NAME  PIC X(16).

      * Request structure for Operation getRedbook
       COPY RBK00Q01.

      * Request structure for Operation createRedbook
       COPY RBK01Q01.

      * Request structure for Operation patchRedbook
       COPY RBK02Q01.

      * Request structure for Operation mergeRedbook
       COPY RBK03Q01.

      * Request structure for Operation getAllRedbooks
       COPY RBK04Q01.

      * Set DEBUG state, 1 for Tracing, 0 without.
       01 WS-DEBUG           PIC 9 COMP VALUE 0.

      * The address of a returned Data Area Element
       01 WS-ELEMENT         USAGE POINTER VALUE NULL.

      * Length of element for BAQGETN call.
       01 WS-ELEMENT-LENGTH  PIC 9(9) COMP-5.

      * Prepare to print messages to the log
       01 WS-FAIL-TYPE       PIC X(18) VALUE SPACES.
       01 WS-CC9             PIC 9(5).
       01 WS-RC9             PIC 9(5).
       01 WS-ST9             PIC 9(5).

      * General Index Counts
       01 WS-INDEX           PIC 9(9).
       01 WS-INDEX-2         PIC 9(9).

      * Display this message to standard out
       01 WS-DISPLAY-MSG     PIC X(78) VALUE ALL SPACES.

      * HTTP Status code
       01 WS-STATUS-CODE     PIC 9(8).

      * General Return Code to track success through execution
       01 WS-RC              PIC 9 VALUE 0.
       77 OK                 PIC 9 VALUE 0.
       77 FAILED             PIC 9 VALUE 1.

      * Call return code
       01 WS-BAQ-RC          PIC 9(8) COMP-5.


       LINKAGE SECTION.

      * Handle parameters passed in from JCL
       01  PARM-BUFFER.
          03  PARM-LENGTH       PIC S9(4) COMP.
          03  PARM-DATA.
             05 OPERATION       PIC X(4).
             05 FILLER          PIC X(1).
             05 DEBUG           PIC X(5) VALUE SPACES.
             05 FILLER          PIC X(90).

      * The API Response data structures are specified within the
      * LINKAGE-SECTION as the Host API owns and manages the storage
      * and not the application program.

      * Response structure for Operation getRedbook
       COPY RBK00P01.

      * Response structure for Operation createRedbook
       COPY RBK01P01.

      * Response structure for Operation patchRedbook
       COPY RBK02P01.

      * Response structure for Operation mergeRedbook
       COPY RBK03P01.

      * Response structure for Operation getAllRedbooks
       COPY RBK04P01.

       PROCEDURE DIVISION USING PARM-BUFFER.
      *----------------------------------------------------------------*
      * A-MAINLINE
      *----------------------------------------------------------------*
       A-MAINLINE SECTION.
       A-010.

           IF PARM-LENGTH LESS THAN 4 THEN
              DISPLAY 'PLEASE SPECIFY OPERATION TO CALL AS PARAMETER'
              DISPLAY 'VALID OPERATIONS ARE GARB, GRBK, CRBK, PRBK'
                  ' and MRBK.'
              STOP RUN
           END-IF.

           IF DEBUG = 'DEBUG' THEN
              MOVE 1 TO WS-DEBUG.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' A-MAINLINE Entry.'.

      * Initialise the Host API and acquire a connection to
      * a z/OS Connect server instance
           PERFORM B-INIT.

      * If a connection was gained make a BAQEXEC call to a
      * remote endpoint API operation
           IF WS-RC = OK
              PERFORM C-EXECUTE

      * Free any resources used by BAQEXEC
              PERFORM X-FREE

      * Terminate the connection to the z/OS Connect server
              PERFORM X-TERM
           END-IF.

       A-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' A-MAINLINE Exit. WS-RC=' WS-RC.

           STOP RUN.

      *----------------------------------------------------------------*
      * B-INIT
      *
      * Initialise the program to call the API
      *----------------------------------------------------------------*
       B-INIT SECTION.
       B-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' B-INIT Entry.'

           MOVE OK TO WS-RC.

      * Initialise the Host API and get a connection to the z/OS Connect
      * server
           PERFORM X-INIT.

       B-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' B-INIT Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * C-EXECUTE
      *
      * Call the required API operation
      *----------------------------------------------------------------*
       C-EXECUTE SECTION.
       C-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' C-EXECUTE Entry.'.

      *    GARB
           IF OPERATION = 'GARB' THEN
              PERFORM CA-GET-ALL-REDBOOKS

      *    GRBK Title[, author ]
           ELSE IF OPERATION = 'GRBK' THEN
              PERFORM CB-GET-REDBOOK

      *    CRBK Title
           ELSE IF OPERATION = 'CRBK' THEN
              PERFORM CC-CREATE-REDBOOK

      *    PRBK Title
           ELSE IF OPERATION = 'PRBK' THEN
              PERFORM CD-PATCH-REDBOOK

      *    MRBK Title
           ELSE IF OPERATION = 'MRBK' THEN
              PERFORM CE-MERGE-REDBOOK

      *    Unknown request
           ELSE
              DISPLAY 'Operation ' OPERATION ' UNKNOWN'
              MOVE FAILED TO WS-RC
           END-IF.

       C-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' C-EXECUTE Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CA-GET-ALL-REDBOOKS
      *
      * Operation getAllRedbooks
      *
      * Sets the content of the BAQBASE-RBK04Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * API End Point (EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK04P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is addressed and processed.
      *----------------------------------------------------------------*
       CA-GET-ALL-REDBOOKS SECTION.
       CA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CA-GET-ALL-REDBOOKS Entry.'.

      * Prepare the request
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK04Q01.
           MOVE LENGTH OF BAQBASE-RBK04Q01 TO BAQ-REQ-BASE-LENGTH.

      * For this request we want to get all Redbook Inventory
      * and not the inventory for a particular author so we set
      * the Xauthor-existence flag to 0 to tell z/OS Connect that
      * the optional author parameter is not set.
      *
      * Ever wondered why some generated fields are prefix with 'X'?
      * It is because, as in this case, a clash exists with the
      * language reserved keyword list. AUTHOR is a COBOL keyword.
           MOVE 0 TO Xauthor-existence of BAQBASE-RBK04Q01.

       CA-020.
      * Call the API
      * Passing the address of the API-INFO structure required for the
      * BAQEXEC call. Section X-EXEC is a reusable routine that is
      * used for all API calls.
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK04I01.
           PERFORM X-EXEC.

      * Check that the call was successful, if not exit the section
      * Routine X-EXEC has displayed the error responses
           IF BAQ-ERROR THEN
              DISPLAY OPERATION ' CA-GET-ALL-REDBOOKS BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY OPERATION ' CA-GET-ALL-REDBOOKS BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

       CA-030.
      * z/OS Connect has successfully called the remote endpoint API and
      * the API has returned an HTTP status code that was defined in the
      * Open API document for the called operation. This could be an
      * error HTTP status code, but as long as it is defined in the OAS
      * document then z/OS Connect sees this as a successful call so now
      * we must address the returned base structure and interrogate the
      * returned responses in more detail
      *
      * The address of the returned BAQBASE structure is returned in
      * the BAQ-RESPONSE-AREA so set the structure to that address
           SET ADDRESS OF BAQBASE-RBK04P01 to BAQ-RESP-BASE-ADDRESS.

      * For this operation the OAS document defines two responses
      * 200-OK and 404-NOTFOUND, if the remote endpoint application
      * returns any other HTTP status code then a status of BAQ-WARNING
      * is returned and the endpoint response is returned in
      * BAQ-RESP-STATUS-MESSAGE, but only the first 1024 characters.
      *
      * If we have reached here we know the remote endpoint status code
      * is either 404-NOTFOUND or 200-OK. Depending on the status code
      * this determines which Data Area has been returned. See
      * z/OS connect documentation for details on what a Data Area is.
      * In short it is an area of memory that is described by a
      * generated 01 level data structure, it is dynamic in length and
      * used to reference the returned data on a per HTTP status code
      * bases and also for referencing dynamic length arrays.

      * Check the remote endpoint HTTP status code and check that a
      * response was received, let's check the NOTFOUND case first.
           IF BAQ-RESP-STATUS-CODE EQUAL 404 THEN
              IF responseCode404-existence OF BAQBASE-RBK04P01 > 0 THEN

      * The Redbook API provided a RedbookNotFound response body
      * in a Data Area, the name of that Data Area is located in
      * responseCode404-dataarea in the BAQBASE data structure.
      * Set this name in to WS-DATA-AREA-NAME and use the common
      * routines X-GET-DATA-AREA-ELEMENT and set the expected length
      * of the returned data in WS-ELEMENT-LENGTH.
                 MOVE responseCode404-dataarea OF BAQBASE-RBK04P01 TO
                     WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK04P01-responseCode404 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

      * If WS-RC is failed the BAQGETN call failed
                 IF WS-RC = FAILED THEN EXIT

      * BAQGETN has worked and returned the address of the Data Area
      * that contains the RBK04P01-responseCode404 data structure.
      * Lets address that and display the returned message which
      * should indicate that there are no Redbooks in the repository.
      *
      * The RBK04P01-responseCode404 also contains a dynamic array
      * Data Area of authors Redbooks, but for this operation this
      * array is not set
                 SET ADDRESS OF RBK04P01-responseCode404 TO WS-ELEMENT
                 MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
                 STRING OPERATION
                  ' API EP returned HTTP Status Code '
                  WS-STATUS-CODE
                  ' MESSAGE ' Xmessage OF RBK04P01-responseCode404
                      (1:Xmessage-length OF RBK04P01-responseCode404)
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              ELSE
      * 404 was returned but there is no RedbookNotFound response body
                MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
                 STRING OPERATION
                   ' API EP returned HTTP Status Code '
                   WS-STATUS-CODE
                   ' NO Response Body'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              END-IF
           END-IF.

       CA-040.
      * Process each returned Redbook. Rather than a single entity in
      * the returned Data Area in this case the OAS response body is an
      * array so rather than a '-existence' flag we have '-num' count
      * that details how many elements exist in the array and we will
      * use X-GET-DATA-AREA-ELEMENT to fetch each one in turn.
           IF BAQ-RESP-STATUS-CODE = 200 THEN
              IF responseCode200-num OF BAQBASE-RBK04P01 > 0 THEN

                 DISPLAY OPERATION ' Redbook Inventory'

                 PERFORM CAA-GET-EACH-REDBOOK VARYING WS-INDEX
                    FROM 1 BY 1
                    UNTIL WS-INDEX >
                       responseCode200-num OF BAQBASE-RBK04P01 OR
                       WS-RC = FAILED
              ELSE
                 DISPLAY OPERATION
                   ' EXEC API EP - No Redbooks returned'
              END-IF
           END-IF.

       CA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CA-GET-ALL-REDBOOKS Exit. WS-RC='
                      WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CAA-GET-EACH-REDBOOK
      *
      * Gets each book returned by the remote End Point Service by using
      * BAQGETN (Get Next) and displays the Redbook details.
      *----------------------------------------------------------------*
       CAA-GET-EACH-REDBOOK SECTION.
       CAA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CAA-GET-EACH-REDBOOK Entry.'.

           MOVE responseCode200-dataarea OF BAQBASE-RBK04P01 TO
                WS-DATA-AREA-NAME.

           MOVE LENGTH OF RBK04P01-responseCode200 TO
                                   WS-ELEMENT-LENGTH.

           PERFORM X-GET-DATA-AREA-ELEMENT.

           IF WS-RC = FAILED THEN GO TO CAA-999.

      * We have fetched the Redbook from the Data Area so set the
      * address of the 01 level data structure.
           SET ADDRESS OF RBK04P01-responseCode200 to WS-ELEMENT.

      * For simplicity lets display the content of the Redbook data
      * structure
      *
      * Note that optional fields have an '-existence' field to denote
      * if the field exists or not.

           IF WS-DEBUG = 1 THEN
               DISPLAY OPERATION ' Redbook number ' WS-INDEX.

           STRING OPERATION ' Title '
             Xtitle OF RBK04P01-responseCode200
                  (1:Xtitle-length OF RBK04P01-responseCode200)
           DELIMITED BY SIZE
           INTO WS-DISPLAY-MSG

      * Always display the book titles
           DISPLAY WS-DISPLAY-MSG.
           MOVE SPACES TO WS-DISPLAY-MSG.

      * Redbooks typically have more than one author so these are
      * contained in an array in the OAS Redbook schema and thus
      * returned in a dynamic Data Area. If the OAS Schema defined a
      * 'maxItems' and this is less than the property
      * 'inlineMaxOccursLimit', set in the Gradle Plugin options.yaml,
      * then the array would be inlined in the Redbook data structure.
      * But here we use a dynamic length array again.
           PERFORM CAAA-GET-EACH-AUTHOR VARYING WS-INDEX-2
              FROM 1 BY 1
              UNTIL WS-INDEX-2 >
                 authors-num OF RBK04P01-responseCode200 OR
                 WS-RC = FAILED.

           IF WS-DEBUG = 1 THEN
               DISPLAY OPERATION '  Status '
                 Xstatus OF RBK04P01-responseCode200
                      (1:Xstatus-length OF RBK04P01-responseCode200).

           IF publicationDate-existence
                 OF RBK04P01-responseCode200 > 0 THEN

               IF WS-DEBUG = 1 THEN
                   DISPLAY OPERATION '  Publication Date '
                   publicationDate2 OF RBK04P01-responseCode200
                 (1:publicationDate2-length OF RBK04P01-responseCode200)
               END-IF
           END-IF.

       CAA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CAA-GET-EACH-REDBOOK Exit. WS-RC='
                   WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CAAA-GET-EACH-AUTHOR
      *
      * Gets each author returned by the remote API by using
      * BAQGETN (Get Next) and displays the Author.
      *----------------------------------------------------------------*
       CAAA-GET-EACH-AUTHOR SECTION.
       CAAA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CAAA-GET-EACH-AUTHOR Entry.'.

           MOVE authors-dataarea OF RBK04P01-responseCode200 TO
                WS-DATA-AREA-NAME.

           MOVE LENGTH OF RBK04P01-authors TO
                                   WS-ELEMENT-LENGTH.

           PERFORM X-GET-DATA-AREA-ELEMENT.

           IF WS-RC = FAILED THEN GO TO CAAA-999.

      * We have fetched the Author from the Data Area so set the
      * address of the 01 level data structure.
           SET ADDRESS OF RBK04P01-authors to WS-ELEMENT.

           IF WS-DEBUG = 1 THEN
               DISPLAY OPERATION '   Author ' WS-INDEX-2.

           IF WS-DEBUG = 1 THEN
               DISPLAY OPERATION '    '
                 firstName2 OF RBK04P01-authors
                      (1:firstName2-length OF RBK04P01-authors) ' '
                 lastName2 OF RBK04P01-authors
                      (1:lastName2-length OF RBK04P01-authors).

       CAAA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CAAA-GET-EACH-AUTHOR Exit. WS-RC='
                   WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CB-GET-REDBOOK
      *
      * Operation getRedbook
      *
      * Sets the content of the BAQBASE-RBK00Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * API End Point (EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK00P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CB-GET-REDBOOK SECTION.
       CB-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CB-GET-REDBOOK Entry.'.

      * Prepare the request for sending
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK00Q01.
           MOVE LENGTH OF BAQBASE-RBK00Q01 TO BAQ-REQ-BASE-LENGTH.

      * First we make sure the whole request structure is initialised so
      * that the request is only sending intended values
           INITIALIZE BAQBASE-RBK00Q01.

      * Now populate the fields of the request structure with the values
      * for the new book which we are going to create

      * Set the title and title length
           MOVE "Accelerate Mainframe Application Modernization with Hyb
      -    "rid Cloud" TO Xtitle OF BAQBASE-RBK00Q01.
           MOVE 64 TO Xtitle-length OF BAQBASE-RBK00Q01.

       CB-020.
      * Call the API
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK00I01.
           PERFORM X-EXEC.

      * Check that BAQEXEC returned BAQ-SUCCESS and exit if not
           IF BAQ-ERROR THEN
              DISPLAY OPERATION ' CC-GET-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CB-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY OPERATION ' CC-GET-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CB-999
           END-IF.

      * Successful call, address the base structure and check the
      * status code
           SET ADDRESS OF BAQBASE-RBK00P01 to BAQ-RESP-BASE-ADDRESS.
           MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE.

       CB-030.
      * Process a 500 response code, in this case the response
      * will be in BAQ-RESP-STATUS-MESSAGE.
           IF BAQ-RESP-STATUS-CODE EQUAL 500 THEN
               STRING OPERATION
                   ' API EP returned HTTP Status Code '
                   WS-STATUS-CODE
                   '. Internal Server Error.'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

               PERFORM X-WRITE-DISPLAY-MSG

               DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
               MOVE FAILED TO WS-RC
               GO TO CB-999
           END-IF.

       CB-040.
      * Process a 404 response code, in this case the response
      * will be in data structure RBK00P01-responseCode404
      * accessed via its Data Area responseCode404-dataarea of
      * BAQBASE-RBK00P01 using BAQGETN
           IF BAQ-RESP-STATUS-CODE EQUAL 404 THEN
              STRING OPERATION
                  ' API EP returned HTTP Status Code '
                  WS-STATUS-CODE
                  '. Redbook not found.'
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-MSG

               PERFORM X-WRITE-DISPLAY-MSG

      * Display any message returned with the 404
               IF responseCode404-existence OF BAQBASE-RBK00P01 > 0 THEN

                  MOVE responseCode404-dataarea OF BAQBASE-RBK00P01
                     TO WS-DATA-AREA-NAME

                  MOVE LENGTH OF RBK00P01-responseCode404 TO
                     WS-ELEMENT-LENGTH

                  PERFORM X-GET-DATA-AREA-ELEMENT

                  IF WS-RC = FAILED THEN GO TO CB-999 END-IF

      * We have fetched the RBK00P01-responseCode404 structure from
      * the Data Area so set the address
                  SET ADDRESS OF RBK00P01-responseCode404 to WS-ELEMENT

                  IF Xmessage-length OF RBK00P01-responseCode404 > 1
                   THEN
                     STRING OPERATION
                         ' Message '
                         Xmessage OF RBK00P01-responseCode404
                         (1:Xmessage-length OF RBK00P01-responseCode404)
                         DELIMITED BY SIZE
                         INTO WS-DISPLAY-MSG

                     PERFORM X-WRITE-DISPLAY-MSG
                     MOVE FAILED TO WS-RC
                     GO TO CB-999
                  END-IF
               END-IF
           END-IF.

       CB-050.
      * Process the returned Redbook, check the
      * responseCode200-existence is set and if so use
      * responseCode200-dataarea to get the returned Redbook
      * in to data structure RBK00P01-responseCode200 and
      * display the content
           IF BAQ-RESP-STATUS-CODE EQUAL 200 THEN
              STRING OPERATION
                     ' API EP returned HTTP Status Code '
                     WS-STATUS-CODE
                     '. Found Redbook.'
                     DELIMITED BY SIZE
                     INTO WS-DISPLAY-MSG

               PERFORM X-WRITE-DISPLAY-MSG

               IF responseCode200-existence OF BAQBASE-RBK00P01
                 EQUAL 1 THEN

      * A Redbook was returned so let's get the data for it
                 MOVE responseCode200-dataarea OF BAQBASE-RBK00P01 TO
                    WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK00P01-responseCode200 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

                 IF WS-RC = FAILED THEN GO TO CB-999 END-IF

      * We have fetched the Redbook from the Data Area so set the
      * address of the 01 level Redbook data structure.
                 SET ADDRESS OF RBK00P01-responseCode200 to WS-ELEMENT

                 STRING OPERATION ' Title '
                    Xtitle OF RBK00P01-responseCode200
                    (1:Xtitle-length OF RBK00P01-responseCode200)
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG

               END-IF
           END-IF.

       CB-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CB-GET-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CC-CREATE-REDBOOK
      *
      * Operation createRedbook
      *
      * Sets the content of the BAQBASE-RBK01Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * API End Point (EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK01P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CC-CREATE-REDBOOK SECTION.
       CC-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CC-CREATE-REDBOOK Entry.'.

      * First we make sure the whole request structure is initialised so
      * that the request is only sending intended values
           INITIALIZE BAQBASE-RBK01Q01.

      * Now populate the fields of the request structure with the values
      * for the new book which we are going to create

      * Start with the path parameter which will create the new book
           MOVE "Accelerate Mainframe Application Modernization with Hyb
      -    "rid Cloud" TO Xtitle OF BAQBASE-RBK01Q01.
           MOVE 64 TO Xtitle-length OF BAQBASE-RBK01Q01.

      * Now populate the remaining fields of the book structure
           MOVE Xtitle OF BAQBASE-RBK01Q01
              TO Xtitle2 OF BAQBASE-RBK01Q01.
           MOVE Xtitle-length OF BAQBASE-RBK01Q01
              TO Xtitle2-length OF BAQBASE-RBK01Q01

           MOVE "PUBLISHED" TO Xstatus OF BAQBASE-RBK01Q01.
           MOVE 9 TO Xstatus-length OF BAQBASE-RBK01Q01.

           MOVE 1 TO publicationDate-existence OF BAQBASE-RBK01Q01.
           MOVE "2023-03-31T00:00:00Z" TO publicationDate2
              OF BAQBASE-RBK01Q01.
           MOVE 20 TO publicationDate2-length OF BAQBASE-RBK01Q01.

           MOVE "REDP-5705-00" TO formNumber OF BAQBASE-RBK01Q01.

           MOVE 1 TO documentType-existence OF BAQBASE-RBK01Q01.
           MOVE "PDF" TO documentType2 OF BAQBASE-RBK01Q01.
           MOVE 3 TO documentType2-length OF BAQBASE-RBK01Q01.

           MOVE 1 TO sizeMB-existence OF BAQBASE-RBK01Q01.
           MOVE 6.62 TO sizeMB OF BAQBASE-RBK01Q01.

           MOVE 1 TO url-existence OF BAQBASE-RBK01Q01.
           MOVE
              "https://www.redbooks.ibm.com/redpapers/pdfs/redp5705.pdf"
              TO url2 OF BAQBASE-RBK01Q01.
           MOVE 56 TO url2-length OF BAQBASE-RBK01Q01.

           MOVE 10 TO authors-num OF BAQBASE-RBK01Q01.
           MOVE "AUTHOR-DATA-AREA" TO authors-dataarea
                                   OF BAQBASE-RBK01Q01.

       CC-020.
      * We use BAQPUTN to add the authors to the book and this is
      * performed in a separate section.
           PERFORM CCAA-PUT-EACH-AUTHOR.
           IF WS-RC = FAILED THEN GO TO CC-999.

       CC-030.
      * The request data for our new book is now complete and we are
      * ready to send it to the API endpoint.
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK01Q01.
           MOVE LENGTH OF BAQBASE-RBK01Q01 TO BAQ-REQ-BASE-LENGTH.

      * Passing the address of the API-INFO structure required for the
      * BAQEXEC call. Section X-EXEC is a reuseable routine that is
      * used for all API calls.
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK01I01.
           PERFORM X-EXEC.

      * Check that the call was successful, if not exit the section
      * Routine X-EXEC has displayed the error responses
           IF BAQ-ERROR THEN
              DISPLAY OPERATION ' CC-CREATE-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CC-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY OPERATION ' CC-CREATE-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CC-999
           END-IF.

       CC-040.
      * z/OS Connect has successfully called the remote endpoint API and
      * the API has returned an HTTP status code that was defined in the
      * Open API document for the called operation. This could be an
      * error HTTP status code, but as long as it is defined in the OAS
      * document then z/OS Connect sees this as a successful call so now
      * we must address the returned base structure and interrogate the
      * returned responses in more detail
      *
      * The address of the returned BAQBASE structure is returned in
      * the BAQ-RESPONSE-AREA so set the structure to that address
           SET ADDRESS OF BAQBASE-RBK01P01 to BAQ-RESP-BASE-ADDRESS.
           MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
      * Check the HTTP status code
           IF BAQ-RESP-STATUS-CODE EQUAL 409 THEN
              STRING OPERATION
                   ' API EP returned HTTP Status Code '
                   WS-STATUS-CODE
                   '. Redbook already exists.'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
           END-IF.

           IF BAQ-RESP-STATUS-CODE IS >= 200 AND IS < 300 THEN
               STRING OPERATION
                   ' API EP returned HTTP Status Code '
                   WS-STATUS-CODE
                   '. Created Redbook.'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
           END-IF.

       CC-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CC-CREATE-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CCAA-PUT-EACH-AUTHOR
      *
      * Puts each author of the book by using the BAQPUTN (Put Next)
      * verb.
      *----------------------------------------------------------------*
       CCAA-PUT-EACH-AUTHOR SECTION.
       CCAA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CCAA-PUT-EACH-AUTHOR Entry.'.

      * Setup the variables which X-PUT-DATA-AREA-ELEMENT will be using
      * to add authors to the authors data area
           SET WS-ELEMENT TO ADDRESS OF RBK01Q01-authors.
           MOVE LENGTH OF RBK01Q01-authors TO WS-ELEMENT-LENGTH.
           MOVE authors-dataarea OF BAQBASE-RBK01Q01
                                 TO WS-DATA-AREA-NAME.

      * Now add the authors to the request Data Area
           MOVE 5 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Skyla" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 6 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Loomis" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 4 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Kyle" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 7 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Charlet" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 5 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Suman" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 8 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Gopinath" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 5 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Peter" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 9 TO lastName2-length OF RBK01Q01-authors.
           MOVE "McCaffrey" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 3 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Tim" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 6 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Brooks" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 7 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Juergen" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 5 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Holtz" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 6 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Bryant" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 11 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Panyarachun" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 5 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Purvi" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 5 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Patel" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 7 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Mythili" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 15 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Venkatakrishnan" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

           MOVE 7 TO firstName2-length OF RBK01Q01-authors.
           MOVE "Yichong" TO firstName2 OF RBK01Q01-authors.
           MOVE 1 TO firstName-existence OF RBK01Q01-authors.
           MOVE 2 TO lastName2-length OF RBK01Q01-authors.
           MOVE "Yu" TO lastName2 OF RBK01Q01-authors.
           MOVE 1 TO lastName-existence OF RBK01Q01-authors.
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CCAA-999.

       CCAA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CCAA-PUT-EACH-AUTHOR. WS-RC='
                   WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CD-PATCH-REDBOOK
      *
      * Operation patchRedbook - RFC6902
      *
      * Sets the content of the BAQBASE-RBK02Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * RESTful End Point(EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK02P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CD-PATCH-REDBOOK SECTION.
       CD-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CD-PATCH-REDBOOK Entry.'.

           INITIALIZE BAQBASE-RBK02Q01.

      * Using the PATCH method in an API requests that a server object
      * is updated by a Patch Document on a field by field basis rather
      * then a complete replacement as would happen if using the PUT
      * method.
      *
      * This procedure will invoke the patchRedbook operation in the
      * Redbook management API. Operation patchRedbook uses media-type
      * application/json-patch+json which is defined by RFC 6902.
      * This RFC (https://www.rfc-editor.org/rfc/rfc6902) defines
      * the format of the patch document used to update the entity
      * via the Rest API in a server.
      *
      * We will see in procedure CE-MERGE-REDBOOK the use of a patch
      * document that conforms to RFC 7396.
      * This RFC (https://www.rfc-editor.org/rfc/rfc7396) defines
      * a different patch document format and media-type
      * application/merge-patch+json.
      *
      * The RFC have different advantages and disadvantages,
      * z/OS Connect supports both standards and your API will
      * choose which to employ via the OpenApi definition of the API.
      *
      * The patch document is constructed by z/OS Connect based on the
      * settings made to the BAQBASE-RBK02Q01 request structure. The
      * COBOL Copybook used for both RFC are similar, but the array
      * support is limited in RFC 7396.
      *
      * Once z/OS Connect has constructed the patch document conforming
      * to the requested media-type (RFC) from the binary COBOL
      * structure this patch document is sent to the Rest API
      * endpoint service. This is received at its path/Patch function
      * and using appropriate function the patch document can be applied
      * to an entity.  See method patchRedbook in the Java class
      * RedbookResource.java for a Java example that uses
      * Jakarta JsonPatch classes to update a Java Redbook object.
      *
      * In this procedure we are going to set up a request that will
      * produce a patch document that will perform a number of updates
      * on a Redbook object. So for Redbook
      * 'ABCs of IBM zOS System Programming Volume 1' we are going
      * to update the Redbook to represent a fictitious new version of
      * the Redbook.
      *
      * 1. Update the URL of the Redbooks location.
      * 2. Update the owning departments Contact.
      * 3. Remove an author as the section has been replaced.
      * 4. Add a new Author to the Authors array
      * 5. Delete the size property as it is not used
      * 6. Add a 'version' property dynamically and set it to 2.
      *
      * These updates show a selection of Patch goals and how we
      * manipulate the operations Request structure to achieve these
      * goals.
      *
      * Lets setup the Request structure BAQBASE-RBK02Q01 then
      * make the call to z/OS Connect via the Host API verbs.

      * 1. Update the URL of the redbook location
      *
      * Each property defined in the JSON Schema object when translated
      * to COBOL has a field of the same (or similar) name with the
      * suffix -patch-operation for a simple field or sub structure
      * or -patch-item for an array structure.
      *
      * In these fields we set the operation that we want to perform
      * on a particular field, if a -patch-operation field contains a
      * space then no update is made to the property on the Rest API
      * server.  See Patch product documentation for full details.
      *
      * Set the value 'U' for update to the url-patch-operation field
      * and set the new data.
           MOVE 'U' TO url-patch-operation OF BAQBASE-RBK02Q01.
           MOVE 13 to url-length OF BAQBASE-RBK02Q01.
           MOVE 'http://newurl' TO url OF BAQBASE-RBK02Q01.

      * 2. Update the owning departments Contact.
      *
      * owningDepartment is a property of type object so this is an
      * example of updating an object within an object.
      *
      * Set the value 'U' for update to the contact-patch-operation
      * field and set the new data for contact.
      *
      * If we wanted to update the whole owningDepartment we could
      * set 'U' in the field ngDepartment-patch-operation and then
      * set the data in each field and the whole property object
      * will update.
      *
      * Note the owningDepartment-patch-operation field has
      * been truncated as the field name is too long to fit. To
      * shorten the -patch-operation to -pchop set the Requester
      * Gradle plugin option shortSuffix: yes. The field will then
      * generate as owningDepartment-pchop.  See the product
      * documentation for details on all the suffixes affected by
      * this option.
           MOVE 'U' TO contact-patch-operation OF BAQBASE-RBK02Q01.
           MOVE 14 to contact-length OF BAQBASE-RBK02Q01.
           MOVE 'A. Contact' TO contact OF BAQBASE-RBK02Q01.

      * 3. Remove an author as the section has been replaced.
      *
      * The Author property is of type array and the Redbook has a
      * number of authors. In this example update we want to remove
      * author 'Luiz Fadel' from the Redbook.
      *
      * Any array update is controlled by a field suffixed -patch-item
      * (or -pchitm if option shortSuffix is set to yes).
      * For our Author array we want to remove the second author 'Luiz'
      * We set authors-patch-item to '2' to state we want to change the
      * second array element.
      *
      * -patch-item fields contain either a single array item number
      * or a list of array item numbers that are to be updated. Numbers
      * can be specified as ranges. See the product documentation for
      * full details.
      *
      * We set the authors-num field to 1 to state there is one array
      * element that we want to process. Any -num field should equal the
      * number of elements specified in the corresponding -patch-item
      * field. e.g. if authors-patch-item was '1, 5-7' then
      * authors-num would be 4.
      *
      * If we wanted to update the last name of the author we would set
      * 'U' in the last-name-patch-operation field and supply a new
      * name. Here we want to delete the entire Author item so we set
      * each -patch-operation field in the sub structure to 'D' to
      * state the entire array item is to be deleted.
           MOVE '2' TO authors-patch-item OF BAQBASE-RBK02Q01.
           MOVE 1 TO authors-num OF BAQBASE-RBK02Q01.
           MOVE "AUTHOR-DATA-AREA" TO authors-dataarea
                                              OF BAQBASE-RBK02Q01.


           INITIALIZE RBK02Q01-authors.
           MOVE 'D' TO firstName-patch-operation OF RBK02Q01-authors.
           MOVE 'D' TO lastName-patch-operation OF RBK02Q01-authors.

           SET WS-ELEMENT TO ADDRESS OF RBK02Q01-authors.
           MOVE LENGTH OF RBK02Q01-authors TO WS-ELEMENT-LENGTH.
           MOVE authors-dataarea OF BAQBASE-RBK02Q01
                                    TO WS-DATA-AREA-NAME.

      * Now add the delete author element to the request Data Area
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CD-999.

      * 4. Add a new Author to the Authors array
      *
      * We also want to add a new author to the array so we want to
      * change the fields above slightly.  To specify a new array
      * item we use the '+' symbol as the last element of the
      * -patch-item list. So now we have set up the array to delete
      * one author and add another.  When adding a new array element
      * the -patch-operation fields do not need to be set to any value.
           MOVE '2,+' TO authors-patch-item OF BAQBASE-RBK02Q01.
           MOVE 2 to authors-num OF BAQBASE-RBK02Q01.

           INITIALIZE RBK02Q01-authors.
           MOVE 'New' TO firstName OF RBK02Q01-authors.
           Move 3 TO firstName-length OF RBK02Q01-authors.
           MOVE 'Author' TO lastName OF RBK02Q01-authors.
           Move 6 TO lastName-length OF RBK02Q01-authors.

      * Now add the next author element to the request Data Area
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CD-999.

      * 5. Delete the size property as it is not used
      *
      * To remove a property from an object we set its -patch-operation
      * field to 'D' for delete.
           MOVE 'D' TO sizeMB-patch-operation OF BAQBASE-RBK02Q01.

      * 6. Add a 'version' property dynamically and set it to 2.
      *
      * To be able to add new properties dynamically to an existing
      * object the objects schema must specify
      * additionalProperties: true in the OpenApi operation RequestBody
      * schema definition.
      *
      * This setting states the schema object can have extra properties
      * that are not defined by the schema.  Typically these are stored
      * in a Map or Dictionary defined in the Rest API server entity
      * class.
      *
      * For PATCH the way we specify additional properties in COBOL is
      * no different from a PUT or POST operation that supports
      * additional properties, however the generated Patch document
      * consumed by the Rest API is different.

      * We have now specified a number of field updates in the
      * BAQBASE-RBK02Q01 Request structure we can now make the call to
      * z/OS Connect via the Host API verbs to process the structure
      * and call the endpoint Rest API with a Patch document which will
      * be processed by the API.
      *
      * Here we have chosen to group a number of updates together and
      * process in one call, if desired, each update could be done
      * individually by setting the BAQBASE-RBK02Q01 fields then calling
      * BAQEXEC to call z/OS Connect, then initialize the
      * BAQBASE-RBK02Q01 structure again, update as appropriate and
      * call BAQEXEC again.
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK02Q01.
           MOVE LENGTH OF BAQBASE-RBK02Q01 TO BAQ-REQ-BASE-LENGTH.

           MOVE 'ABCs of IBM zOS System Programming Volume 1'
                   TO Xtitle OF BAQBASE-RBK02Q01.
           MOVE 44 to Xtitle-length OF BAQBASE-RBK02Q01.

       CD-020.
      * Call the API
      * Passing the address of the API-INFO structure required for the
      * BAQEXEC call. Section X-EXEC is a reusable routine that is
      * used for all API calls.
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK02I01.
           PERFORM X-EXEC.

      * Check that the call was successful, if not exit the section
      * Routine X-EXEC has displayed the error responses
           IF BAQ-ERROR THEN
              DISPLAY OPERATION ' CD-PATCH-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY OPERATION ' CD-PATCH-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

       CD-030.
      * The BAQHAPI has successfully called remote endpoint API and that
      * API has returned a HTTP status code that was defined in the
      * Open API document for the called operation.  This could be an
      * error HTTP status code, but as long as it is defined in the OAS
      * document then BAQHAPI sees this as a successful call so now we
      * must address the returned base structure and interrogate the
      * returned responses in more detail
      *
      * The address of the returned BAQBASE structure is returned in
      * the BAQ-RESPONSE-AREA so set the structure to that address
           SET ADDRESS OF BAQBASE-RBK02P01 to BAQ-RESP-BASE-ADDRESS.

      * For this operation the OAS document defines two responses
      * 200-OK and 404-NOTFOUND, if the remote endpoint application
      * returns any other HTTP status code then a status of BAQ-WARNING
      * is returned and the endpoint response is returned in
      * BAQ-RESP-STATUS-MESSAGE, first 1024 characters.
      *
      * If we have reached here we know the remote endpoint status code
      * is either 404-NOTFOUND or 200-OK. Depending on the status code
      * this determines which Data Area has been returned. See
      * z/OS connect documentation for details on what a Data Area is.
      * In short it is an area of memory that is described by a
      * generated 01 level data structure, it is dynamic in length and
      * used to reference the returned data on a per HTTP status code
      * bases and also for referencing dynamic length arrays.

      * Check the remote endpoint HTTP status code and check that a
      * response was received, lets do the NOTFOUND case first.
           IF BAQ-RESP-STATUS-CODE EQUAL 404 THEN
              IF responseCode404-existence OF BAQBASE-RBK02P01 > 0 THEN

      * The Redbook API provided a RedbookNotFound response body
      * in a Data Area, the name of that Data Area is located in
      * responseCode404-dataarea in the BAQBASE data structure.
      * Set this name in to WS-DATA-AREA-NAME and use the common
      * routines X-GET-DATA-AREA-ELEMENT and set the expected length
      * of the returned data in WS-ELEMENT-LENGTH.
                 MOVE responseCode404-dataarea OF BAQBASE-RBK02P01 TO
                     WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK02P01-responseCode404 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

      * If WS-RC is failed the BAQGETN call failed
                 IF WS-RC = FAILED THEN EXIT

      * BAQGETN has worked and returned the address of the Data Area
      * that contains the RBK04P01-responseCode404 data structure.
      * Lets address that and display the returned message which
      * should indicate that there are no Red Books in the repository.
      *
      * The RBK04P01-responseCode404 also contains a dynamic array
      * Data Area of authors Red Books, but for this operation this
      * array is not set
                 SET ADDRESS OF RBK02P01-responseCode404 TO WS-ELEMENT
                 MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
                 STRING OPERATION
                  ' EXEC RESTful EP return HTTP Status Code '
                  WS-STATUS-CODE
                  ' MESSAGE ' Xmessage OF RBK02P01-responseCode404
                      (1:Xmessage-length OF RBK02P01-responseCode404)
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              ELSE
      * 404 was returned but there is no RedbookNotFound response body
                 STRING OPERATION
                   ' EXEC RESTful EP return HTTP Status Code '
                   WS-STATUS-CODE
                   ' NO Response Body'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              END-IF
           END-IF.

       CD-040.
      * Process the returned Redbook.
           IF BAQ-RESP-STATUS-CODE = 200 THEN
              IF responseCode200-existence OF BAQBASE-RBK02P01 > 0 THEN

                 DISPLAY OPERATION ' Patched Red Book received'
                 MOVE responseCode200-dataarea OF BAQBASE-RBK02P01 TO
                     WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK02P01-responseCode200 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

      * BAQGETN has worked and returned the address of the Data Area
      * that contains the RBK02P01-responseCode200 data structure.
                 SET ADDRESS OF RBK02P01-responseCode200 TO WS-ELEMENT

      * Check the fields have been updated by displaying the values
                 STRING OPERATION ' URL is now '
                   url2 OF RBK02P01-responseCode200
                        (1:url2-length OF RBK02P01-responseCode200)
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG
                 DISPLAY WS-DISPLAY-MSG
                 MOVE SPACES TO WS-DISPLAY-MSG

                 STRING OPERATION ' contact is now '
                   contact OF RBK02P01-responseCode200
                        (1:contact-length OF RBK02P01-responseCode200)
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG
                 DISPLAY WS-DISPLAY-MSG
                 MOVE SPACES TO WS-DISPLAY-MSG

                 DISPLAY OPERATION ' Number of authors is '
                       authors-num OF RBK02P01-responseCode200

                 PERFORM VARYING WS-INDEX
                    FROM 1 BY 1
                    UNTIL WS-INDEX >
                       authors-num OF RBK02P01-responseCode200

                    MOVE authors-dataarea OF RBK02P01-responseCode200
                        TO WS-DATA-AREA-NAME

                    MOVE LENGTH OF RBK02P01-authors TO WS-ELEMENT-LENGTH

                    PERFORM X-GET-DATA-AREA-ELEMENT

                    IF WS-RC = FAILED THEN GO TO CD-999 END-IF

      * We have fetched the Author from the Data Area so set the
      * address of the 01 level data structure.
                   SET ADDRESS OF RBK02P01-authors TO WS-ELEMENT

                   IF WS-DEBUG = 1 THEN
                      DISPLAY OPERATION '   Author ' WS-INDEX

                    STRING OPERATION ' Author first name '
                         firstName2 OF RBK02P01-authors
                       (1:firstName2-length
                               OF RBK02P01-authors)
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MSG
                    DISPLAY WS-DISPLAY-MSG
                    MOVE SPACES TO WS-DISPLAY-MSG

                    STRING OPERATION ' Author last name '
                        lastName2 OF RBK02P01-authors
                       (1:lastName2-length
                               OF RBK02P01-authors)
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MSG
                    DISPLAY WS-DISPLAY-MSG
                    MOVE SPACES TO WS-DISPLAY-MSG
                   END-IF
                 END-PERFORM

                 DISPLAY OPERATION ' sizeMB-existence is '
                           sizeMB-existence OF RBK02P01-responseCode200

      *           DISPLAY OPERATION ' additional property is '
      *               responseBody-json-property OF BAQBASE-RBK02Q01(1)
              END-IF.

       CD-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CD-PATCH-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CE-MERGE-REDBOOK
      *
      * Operation mergeRedbook - RFC7396
      *
      * Sets the content of the BAQBASE-RBK03Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * RESTful End Point(EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK03P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CE-MERGE-REDBOOK SECTION.
       CE-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CE-MERGE-REDBOOK Entry.'.

           INITIALIZE BAQBASE-RBK03Q01.

      * Using the PATCH method in an API requests that a server object
      * is updated by a Patch Document on a field by field basis rather
      * then a complete replacement as would happen if using the PUT
      * method.
      *
      * This procedure will invoke the mergeRedbook operation in the
      * Redbook management API. Operation mergeRedbook uses media-type
      * application/merge-patch+json which is defined by RFC 7396.
      * This RFC (https://www.rfc-editor.org/rfc/rfc7396) defines
      * the format of the patch document used to update the entity
      * via the Rest API in a server.
      *
      * The patch document is constructed by z/OS Connect based on the
      * settings made to the BAQBASE-RBK03Q01 request structure.
      *
      * Once z/OS Connect has constructed the patch document conforming
      * to the requested media-type (RFC) from the binary COBOL
      * structure this patch document is sent to the Rest API
      * endpoint service. This is received at its path/Patch function
      * and using appropriate function the patch document can be applied
      * to an entity.  See method mergeRedbook in the Java class
      * RedbookResource.java for a Java example that uses
      * Jakarta JsonPatch classes to update a Java Redbook object.
      *
      * In this procedure we are going to set up a request that will
      * produce a patch document that will perform a number of updates
      * on a Redbook object. So for Redbook
      * 'ABCs of IBM z/OS System Programming Volume 2' we are going
      * to update the Redbook to represent a ficticious new version of
      * the Redbook.
      *
      * 1. Update the URL of the Redbooks location.
      * 2. Update the owning departments Contact.
      * 3. Replace the Authors array.
      * 4. Delete the size property as it is not used
      * 5. Add a 'version' property dynamically and set it to 2.
      *
      * These updates show a selection of Patch goals and how we
      * manipulate the operations Request structure to achieve these
      * goals.
      *
      * Lets setup the Request structure BAQBASE-RBK03Q01 then
      * make the call to z/OS Connect via the Host API verbs.

      * 1. Update the URL of the redbook location
      *
      * Each property defined in the JSON Schema object when translated
      * to COBOL has a field of the same (or similar) name with the
      * suffix -patch-operation for a simple field or sub structure
      * or -patch-item for an array structure.
      *
      * In these fields we set the operation that we want to perform
      * on a particular field, if a -patch-operation field contains a
      * space then no update is made to the property on the Rest API
      * server.  See Patch product documentation for full details.
      *
      * Set the value 'U' for update to the url-patch-operation field
      * and set the new data.
           MOVE 'U' TO url-patch-operation OF BAQBASE-RBK03Q01.
           MOVE 13 to url-length OF BAQBASE-RBK03Q01.
           MOVE 'http://newurl' TO url OF BAQBASE-RBK03Q01.

      * 2. Update the owning departments Contact.
      *
      * owningDepartment is a property of type object so this is an
      * example of updating an object within an object.
      *
      * Set the value 'U' for update to the contact-patch-operation
      * field and set the new data for contact.
      *
      * If we wanted to update the whole owningDepartment we could
      * set 'U' in the field ngDepartment-patch-operation and then
      * set the data in each field and the whole property object
      * will update.
      *
      * Note the owningDepartment-patch-operation field has
      * been truncated as the field name is too long to fit. To
      * shorten the -patch-operation to -pchop set the Requester
      * Gradle plugin option shortSuffix: yes. The field will then
      * generate as owningDepartment-pchop.  See the product
      * documentation for details on all the suffixes affected by
      * this option.
           MOVE 'U' TO contact-patch-operation OF BAQBASE-RBK03Q01.
           MOVE 14 to contact-length OF BAQBASE-RBK03Q01.
           MOVE 'A. Contact' TO contact OF BAQBASE-RBK03Q01.

      * 3. Replace the Authors array
      *
      * With RFC 7396 the array manipulation capabilities are limited,
      * the whole array property can be replaced or the whole array
      * property can be deleted.
      *
      * Here we are going to supply new array content with a single
      * author.
      *
      * In RFC 7396, like the non-array fields, the array has a
      * correspond -patch-operation field, in this case
      * authors-patch-operation.
      *
      * We set the authors-num field to 1 to state there is one array
      * element in the replaced array content
           MOVE 'U' TO authors-patch-operation OF BAQBASE-RBK03Q01.
           MOVE 1 TO authors-num OF BAQBASE-RBK03Q01.
           MOVE "AUTHOR-DATA-AREA" TO authors-dataarea
                                   OF BAQBASE-RBK03Q01.

           INITIALIZE RBK03Q01-authors.

           MOVE 1 TO firstName-existence OF RBK03Q01-authors.
           MOVE "New" TO firstName2 OF RBK03Q01-authors.
           MOVE 3 TO firstName2-length OF RBK03Q01-authors.
           MOVE 1 TO lastName-existence OF RBK03Q01-authors.
           MOVE "Author" TO lastName2 OF RBK03Q01-authors.
           MOVE 6 TO lastName2-length OF RBK03Q01-authors.

           SET WS-ELEMENT TO ADDRESS OF RBK03Q01-authors.
           MOVE LENGTH OF RBK03Q01-authors TO WS-ELEMENT-LENGTH.
           MOVE authors-dataarea OF BAQBASE-RBK03Q01
                                    TO WS-DATA-AREA-NAME.

      * Now add the delete author element to the request Data Area
           PERFORM X-PUT-DATA-AREA-ELEMENT.
           IF WS-RC = FAILED THEN GO TO CE-999.

      * 4. Delete the size property as it is not used
      *
      * To remove a property from an object we set its -patch-operation
      * field to 'D' for delete.
           MOVE 'D' TO sizeMB-patch-operation OF BAQBASE-RBK03Q01.

      * 5. Add a 'version' property dynamically and set it to 2.
      *
      * To be able to add new properties dynamically to an existing
      * object the objects schema must specify
      * additionalProperties: true in the OpenApi operation RequestBody
      * schema definition.
      *
      * This setting states the schema object can have extra properties
      * that are not defined by the schema.  Typically these are stored
      * in a Map or Dictionary defined in the Rest API server entity
      * class.
      *
      * For PATCH the way we specify additional properties in COBOL is
      * no different from a PUT or POST operation that supports
      * additional properties, however the generated Patch document
      * consumed by the Rest API is different.

      * We have now specified a number of field updates in the
      * BAQBASE-RBK03Q01 Request structure we can now make the call to
      * z/OS Connect via the Host API verbs to process the structure
      * and call the endpoint Rest API with a Patch document which will
      * be processed by the API.
      *
      * Here we have chosen to group a number of updates together and
      * process in one call, if desired, each update could be done
      * individually by setting the BAQBASE-RBK03Q01 fields then calling
      * BAQEXEC to call z/OS Connect, then initialize the
      * BAQBASE-RBK03Q01 structure again, update as appropriate and
      * call BAQEXEC again.
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK03Q01.
           MOVE LENGTH OF BAQBASE-RBK03Q01 TO BAQ-REQ-BASE-LENGTH.

           MOVE 'ABCs of IBM zOS System Programming Volume 2'
                   TO Xtitle OF BAQBASE-RBK03Q01.
           MOVE 44 to Xtitle-length OF BAQBASE-RBK03Q01.

       CE-020.
      * Call the API
      * Passing the address of the API-INFO structure required for the
      * BAQEXEC call. Section X-EXEC is a reusable routine that is
      * used for all API calls.
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK03I01.
           PERFORM X-EXEC.

      * Check that the call was successful, if not exit the section
      * Routine X-EXEC has displayed the error responses
           IF BAQ-ERROR THEN
              DISPLAY OPERATION ' CE-MERGE-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY OPERATION ' CE-MERGE-REDBOOK BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

       CE-030.
      * The BAQHAPI has successfully called remote endpoint API and that
      * API has returned a HTTP status code that was defined in the
      * Open API document for the called operation.  This could be an
      * error HTTP status code, but as long as it is defined in the OAS
      * document then BAQHAPI sees this as a successful call so now we
      * must address the returned base structure and interrogate the
      * returned responses in more detail
      *
      * The address of the returned BAQBASE structure is returned in
      * the BAQ-RESPONSE-AREA so set the structure to that address
           SET ADDRESS OF BAQBASE-RBK03P01 to BAQ-RESP-BASE-ADDRESS.

      * For this operation the OAS document defines two responses
      * 200-OK and 404-NOTFOUND, if the remote endpoint application
      * returns any other HTTP status code then a status of BAQ-WARNING
      * is returned and the endpoint response is returned in
      * BAQ-RESP-STATUS-MESSAGE, first 1024 characters.
      *
      * If we have reached here we know the remote endpoint status code
      * is either 404-NOTFOUND or 200-OK. Depending on the status code
      * this determines which Data Area has been returned. See
      * z/OS connect documentation for details on what a Data Area is.
      * In short it is an area of memory that is described by a
      * generated 01 level data structure, it is dynamic in length and
      * used to reference the returned data on a per HTTP status code
      * bases and also for referencing dynamic length arrays.

      * Check the remote endpoint HTTP status code and check that a
      * response was received, lets do the NOTFOUND case first.
           IF BAQ-RESP-STATUS-CODE EQUAL 404 THEN
              IF responseCode404-existence OF BAQBASE-RBK03P01 > 0 THEN

      * The Redbook API provided a RedbookNotFound response body
      * in a Data Area, the name of that Data Area is located in
      * responseCode404-dataarea in the BAQBASE data structure.
      * Set this name in to WS-DATA-AREA-NAME and use the common
      * routines X-GET-DATA-AREA-ELEMENT and set the expected length
      * of the returned data in WS-ELEMENT-LENGTH.
                 MOVE responseCode404-dataarea OF BAQBASE-RBK03P01 TO
                     WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK03P01-responseCode404 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

      * If WS-RC is failed the BAQGETN call failed
                 IF WS-RC = FAILED THEN EXIT

      * BAQGETN has worked and returned the address of the Data Area
      * that contains the RBK04P01-responseCode404 data structure.
      * Lets address that and display the returned message which
      * should indicate that there are no Red Books in the repository.
      *
      * The RBK04P01-responseCode404 also contains a dynamic array
      * Data Area of authors Red Books, but for this operation this
      * array is not set
                 SET ADDRESS OF RBK03P01-responseCode404 TO WS-ELEMENT
                 MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
                 STRING OPERATION
                  ' EXEC RESTful EP return HTTP Status Code '
                  WS-STATUS-CODE
                  ' MESSAGE ' Xmessage OF RBK03P01-responseCode404
                      (1:Xmessage-length OF RBK03P01-responseCode404)
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              ELSE
      * 404 was returned but there is no RedbookNotFound response body
                 STRING OPERATION
                   ' EXEC RESTful EP return HTTP Status Code '
                   WS-STATUS-CODE
                   ' NO Response Body'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-DISPLAY-MSG
              END-IF
           END-IF.

       CE-040.
      * Process the returned Redbook.
           IF BAQ-RESP-STATUS-CODE = 200 THEN
              IF responseCode200-existence OF BAQBASE-RBK03P01 > 0 THEN

                 DISPLAY OPERATION ' Merged Red Book received'
                 MOVE responseCode200-dataarea OF BAQBASE-RBK03P01 TO
                     WS-DATA-AREA-NAME

                 MOVE LENGTH OF RBK03P01-responseCode200 TO
                    WS-ELEMENT-LENGTH

                 PERFORM X-GET-DATA-AREA-ELEMENT

      * BAQGETN has worked and returned the address of the Data Area
      * that contains the RBK03P01-responseCode200 data structure.
                 SET ADDRESS OF RBK03P01-responseCode200 TO WS-ELEMENT

      * Check the fields have been updated by displaying the values
                 STRING OPERATION ' URL is now '
                   url2 OF RBK03P01-responseCode200
                        (1:url2-length OF RBK03P01-responseCode200)
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG
                 DISPLAY WS-DISPLAY-MSG
                 MOVE SPACES TO WS-DISPLAY-MSG

                 STRING OPERATION ' contact is now '
                   contact OF RBK03P01-responseCode200
                        (1:contact-length OF RBK03P01-responseCode200)
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG
                 DISPLAY WS-DISPLAY-MSG
                 MOVE SPACES TO WS-DISPLAY-MSG

                 DISPLAY OPERATION ' Number of authors is '
                       authors-num OF RBK03P01-responseCode200

                 PERFORM VARYING WS-INDEX
                    FROM 1 BY 1
                    UNTIL WS-INDEX >
                       authors-num OF RBK03P01-responseCode200

                    MOVE authors-dataarea OF RBK03P01-responseCode200
                        TO WS-DATA-AREA-NAME

                    MOVE LENGTH OF RBK03P01-authors TO WS-ELEMENT-LENGTH

                    PERFORM X-GET-DATA-AREA-ELEMENT

                    IF WS-RC = FAILED THEN GO TO CE-999 END-IF

      * We have fetched the Author from the Data Area so set the
      * address of the 01 level data structure.
                   SET ADDRESS OF RBK03P01-authors TO WS-ELEMENT

                   IF WS-DEBUG = 1 THEN
                      DISPLAY OPERATION '   Author ' WS-INDEX

                    STRING OPERATION ' Author first name '
                         firstName2 OF RBK03P01-authors
                       (1:firstName2-length
                               OF RBK03P01-authors)
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MSG
                    DISPLAY WS-DISPLAY-MSG
                    MOVE SPACES TO WS-DISPLAY-MSG

                    STRING OPERATION ' Author last name '
                        lastName2 OF RBK03P01-authors
                       (1:lastName2-length
                               OF RBK03P01-authors)
                    DELIMITED BY SIZE
                    INTO WS-DISPLAY-MSG
                    DISPLAY WS-DISPLAY-MSG
                    MOVE SPACES TO WS-DISPLAY-MSG
                   END-IF
                 END-PERFORM

                 DISPLAY OPERATION ' sizeMB-existence is '
                           sizeMB-existence OF RBK03P01-responseCode200

      *           DISPLAY OPERATION ' additional property is '
      *               responseBody-json-property OF BAQBASE-RBK03Q01(1)
              END-IF.

       CE-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' CD-MERGE-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-INIT
      *
      * Initialize z/OS Connect call by calling BAQINIT this will
      * acquire a connection to a z/OS Connect server and initialise
      * the Host API ready for communication.
      *----------------------------------------------------------------*
       X-INIT SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-INIT Entry.'.

           CALL BAQ-INIT-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY OPERATION ' INIT Return Code '
                WS-CC9
              DISPLAY OPERATION
                      ' INIT See STDOUT/STDERR for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' INIT Completion Code '
                WS-CC9
              DISPLAY OPERATION ' INIT Reason Code '
                WS-RC9.

      * Check for bad initialisation
           IF NOT BAQ-SUCCESS THEN
              MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9
              MOVE BAQ-ZCON-REASON-CODE TO WS-RC9
              STRING OPERATION
                 ' INIT failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)

              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-INIT Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-EXEC
      *
      * Make the BAQEXEC call
      *----------------------------------------------------------------*
       X-EXEC SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-EXEC Entry.'.

           CALL BAQ-EXEC-NAME USING
                              BY REFERENCE BAQ-ZCONNECT-AREA
                              BY VALUE WS-API-INFO
                              BY REFERENCE BAQ-REQUEST-AREA
                              BY REFERENCE BAQ-RESPONSE-AREA
                              RETURNING WS-BAQ-RC.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.
           MOVE BAQ-RESP-STATUS-CODE TO WS-ST9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' EXEC Completion Code '
                WS-CC9
              DISPLAY OPERATION ' EXEC Reason Code '
                WS-RC9
              DISPLAY OPERATION ' EXEC HTTP Status Code '
                WS-ST9.

           IF NOT BAQ-SUCCESS THEN
              EVALUATE TRUE
                 WHEN BAQ-WARNING
                    MOVE 'API RETURN WARNING' TO WS-FAIL-TYPE
                 WHEN BAQ-ERROR
                    MOVE 'API RETURN ERROR  ' TO WS-FAIL-TYPE
                 WHEN BAQ-SEVERE
                    MOVE 'API RETURN SEVERE ' TO WS-FAIL-TYPE
              END-EVALUATE

              STRING OPERATION
                 ' EXEC failed with '
                 WS-FAIL-TYPE
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              IF WS-DEBUG = 1 THEN
                  DISPLAY OPERATION ' ' BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              END-IF
           ELSE
              IF WS-DEBUG = 1 THEN
                 DISPLAY OPERATION ' EXEC Status Code '
                   BAQ-RESP-STATUS-CODE
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-EXEC Exit.'.

           EXIT.

      *----------------------------------------------------------------*
      * X-PUT-DATA-AREA-ELEMENT
      *
      * Puts a Data Element in to the Data Area named in the variable
      * WS-DATA-AREA-NAME using address WS-ELEMENT and length
      * WS-ELEMENT-LENGTH.
      *
      * Calls BAQPUTN (Put Next) which returns WS-BAQ-RC.
      *----------------------------------------------------------------*
       X-PUT-DATA-AREA-ELEMENT SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-PUT-DATA-AREA-ELEMENT '
                 WS-DATA-AREA-NAME
                 ' Entry.'.

           CALL BAQ-PUTN-NAME USING
                              BY REFERENCE BAQ-ZCONNECT-AREA
                              WS-DATA-AREA-NAME
                              BY REFERENCE WS-ELEMENT
                              BY REFERENCE WS-ELEMENT-LENGTH
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE WS-BAQ-RC TO WS-CC9
              MOVE FAILED TO WS-RC
              DISPLAY OPERATION ' PUTN Return Code '
                WS-CC9
              DISPLAY OPERATION ' PUTN See STDOUT/STDERR for details'.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' PUTN Completion Code '
                WS-CC9
              DISPLAY OPERATION ' PUTN Reason Code '
                WS-RC9.

           IF NOT BAQ-SUCCESS THEN
              STRING OPERATION
                 ' PUTN failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-PUT-DATA-AREA-ELEMENT Exit. WS-RC='
                 WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-GET-DATA-AREA-ELEMENT
      *
      * Gets a Data Element from the Data Area named in the variable
      * WS-DATA-AREA-NAME using length WS-ELEMENT-LENGTH.
      *
      * Calls BAQGETN (Get Next) which sets WS-ELEMENT to the address
      * of the data element retrieved.
      *----------------------------------------------------------------*
       X-GET-DATA-AREA-ELEMENT SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-GET-DATA-AREA-ELEMENT '
                 WS-DATA-AREA-NAME
                 ' Entry.'.

           CALL BAQ-GETN-NAME USING
                              BY REFERENCE BAQ-ZCONNECT-AREA
                              WS-DATA-AREA-NAME
                              BY REFERENCE WS-ELEMENT
                              BY REFERENCE WS-ELEMENT-LENGTH
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE WS-BAQ-RC TO WS-CC9
              MOVE FAILED TO WS-RC
              DISPLAY OPERATION ' GETN Return Code '
                WS-CC9
              DISPLAY OPERATION ' GETN See STDOUT/STDERR for details'.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' GETN Completion Code '
                WS-CC9
              DISPLAY OPERATION ' GETN Reason Code '
                WS-RC9.

           IF NOT BAQ-SUCCESS THEN
              STRING OPERATION
                 ' GETN failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-GET-DATA-AREA-ELEMENT Exit. WS-RC='
                 WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-FREE
      *
      * Free any storage used by BAQEXEC
      *----------------------------------------------------------------*
       X-FREE SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-FREE Entry.'.

           CALL BAQ-FREE-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY OPERATION ' FREE Return Code '
                WS-CC9
              DISPLAY OPERATION ' FREE See STDOUT/STDERR for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' FREE Completion Code '
                WS-CC9
              DISPLAY OPERATION ' FREE Reason Code '
                WS-RC9.

      * Check for bad free
           IF NOT BAQ-SUCCESS THEN
              STRING OPERATION
                 ' FREE failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-FREE Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-TERM
      *
      * Terminates the connection to z/OS Connect using BAQTERM.
      *----------------------------------------------------------------*
       X-TERM SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-TERM Entry.'.

      * Terminate the connection
           CALL BAQ-TERM-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY OPERATION ' TERM Return Code '
                WS-CC9
              DISPLAY OPERATION ' TERM See STDOUT/STDERR for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' TERM Completion Code '
                WS-CC9
              DISPLAY OPERATION ' TERM Reason Code '
                WS-RC9.

           IF NOT BAQ-SUCCESS THEN
              STRING OPERATION
                 ' TERM failed'
                 WS-FAIL-TYPE
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-DISPLAY-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY OPERATION ' X-TERM Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * Write messages to standard out
      *----------------------------------------------------------------*
       X-WRITE-DISPLAY-MSG SECTION.
       X-010.
           DISPLAY WS-DISPLAY-MSG.

           MOVE SPACES TO WS-DISPLAY-MSG.

           EXIT.
