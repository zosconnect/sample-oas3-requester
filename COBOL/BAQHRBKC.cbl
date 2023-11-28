      *****************************************************************
      * Copyright IBM Corp. 2023
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
      * BAQHRBKC                                                      *
      *                                                               *
      * Install CSD Group BAQHRBKG                                    *
      *                                                               *
      * Processes Transactions:                                       *
      *      GARB - Get All Redbooks                                  *
      *      GRBK - Get Redbook                                       *
      *      CRBK - Create Redbook                                    *
      *                                                               *
      * Calls RedbookAPI endpoint RESTful Application operations      *
      * to process the Tx request.                                    *
      *                                                               *
      * Copyright IBM Corp. 2023                                      *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BAQHRBKC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * API requester Host API required copy books
       COPY BAQHAREC.
       COPY BAQHCONC.

      * API-INFO for Operation getRedbook
       COPY RBK00I01.

      * API-INFO for Operation createRedbook
       COPY RBK01I01.

      * API-INFO for Operation getAllRedbooks
       COPY RBK02I01.

      * Pointer to API-INFO structure
       01 WS-API-INFO        USAGE POINTER VALUE NULL.

      * Data Area name to get
       01 WS-DATA-AREA-NAME  PIC X(16).

      * Request structure for Operation getRedbook
       COPY RBK00Q01.

      * Request structure for Operation createRedbook
       COPY RBK01Q01.

      * Request structure for Operation getAllRedbooks
       COPY RBK02Q01.

      * Set DEBUG state, 1 for Tracing, 0 without.
       01 WS-DEBUG           PIC 9 COMP VALUE 1.

      * Set WS-URIMAP to the name of a defined CSD URIMAP that
      * can be used to target a particular z/OS Connect instance.
      * Leave as spaces to use the default z/OS Connect URIMAP.
       01 WS-URIMAP          PIC X(8) VALUE SPACES.

      * Tx cmdline input
       01 WS-NETNAME         PIC X(8) VALUE ALL '?'.
       01 WS-TERMINAL-INPUT.
          03 WS-TX           PIC X(4).
          03 FILLER          PIC X(1).
          03 FILLER          PIC X(75) VALUE SPACES.

       01 WS-TERMINAL-LENGTH PIC S9(4) BINARY.

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

      * Display this message to CICS log
       01 WS-DISPLAY-MSG     PIC X(78) VALUE ALL SPACES.

      * HTTP Status code
       01 WS-STATUS-CODE     PIC 9(8).

      * General Return Code to track success through execution
       01 WS-RC              PIC 9 VALUE 0.
       77 OK                 PIC 9 VALUE 0.
       77 FAILED             PIC 9 VALUE 1.

      * BAQ Call return code
       01 WS-BAQ-RC          PIC 9(8) COMP-5.


       LINKAGE SECTION.
      * The API Response data structures are used within the
      * LINKAGE-SECTION as the BAQ Host API owns and manages the storage
      * and not the application program.

      * Response structure for Operation getRedbook
       COPY RBK00P01.

      * Response structure for Operation createRedbook
       COPY RBK01P01.

      * Response structure for Operation getAllRedbooks
       COPY RBK02P01.


       PROCEDURE DIVISION.
      *----------------------------------------------------------------*
      * A-MAINLINE
      *----------------------------------------------------------------*
       A-MAINLINE SECTION.
       A-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' A-MAINLINE Entry.'.

           EXEC CICS ASSIGN
                NETNAME(WS-NETNAME)
           END-EXEC.

           EXEC CICS RECEIVE INTO(WS-TERMINAL-INPUT)
                             LENGTH(WS-TERMINAL-LENGTH)
                             MAXLENGTH(80)
           END-EXEC.

      * Initialise the BAQ Host API and acquire a connection to
      * a z/OS Connect server instance
           PERFORM B-INIT-TX.

      * If a connection was gained execute the Tx and make a BAQEXEC
      * call to a remote endpoint API operation based on the Tx name
           IF WS-RC = OK
              PERFORM C-EXECUTE-TX

      * Free any resources used by BAQEXEC
              PERFORM X-FREE

      * Terminate the BAQHAPI connection to the z/OS Connect server
      * In CICS the allocated connection is returned to a pool
      * and will be reused for the next request to the same z/OS Connect
      * server instance.
              PERFORM X-TERM
           END-IF.

       A-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' A-MAINLINE Exit. WS-RC=' WS-RC.

           IF WS-NETNAME NOT EQUAL ALL '?'
                 EXEC CICS SEND PAGE
                      RETAIN
                      LAST
                      END-EXEC
           END-IF.

           EXEC CICS RETURN END-EXEC.

      *----------------------------------------------------------------*
      * B-INIT-TX
      *
      * Initialise the program to execute the Tx
      *----------------------------------------------------------------*
       B-INIT-TX SECTION.
       B-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' B-INIT-TX Entry.'

           MOVE OK TO WS-RC.

      * Set the CICS ABEND Handler
           EXEC CICS HANDLE ABEND
              LABEL(X-HANDLE-ABEND)
           END-EXEC.

      * Initialise the Host API and get a conection to the z/OS Connect
      * server
           PERFORM X-INIT.

       B-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' B-INIT-TX Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * C-EXECUTE-TX
      *
      * Execute the given Tx
      *----------------------------------------------------------------*
       C-EXECUTE-TX SECTION.
       C-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' C-EXECUTE-TX Entry.'.

      *    GARB
           IF EIBTRNID = 'GARB' THEN
              PERFORM CA-GET-ALL-REDBOOKS

      *    GRBK Title[, author ]
           ELSE IF EIBTRNID = 'GRBK' THEN
              PERFORM CB-GET-REDBOOK

      *    CRBK Title
           ELSE IF EIBTRNID = 'CRBK' THEN
              PERFORM CC-CREATE-REDBOOK

      *    Unknown Tx
           ELSE
              DISPLAY 'Tx ' EIBTRNID ' UNKNOWN'
              MOVE FAILED TO WS-RC
           END-IF.

       C-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' C-EXECUTE-TX Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CA-GET-ALL-REDBOOKS
      *
      * Operation getAllRedbooks
      *
      * Sets the content of the BAQBASE-RBK02Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * RESTful End Point(EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK02P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is addressed and processed.
      *----------------------------------------------------------------*
       CA-GET-ALL-REDBOOKS SECTION.
       CA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CA-GET-ALL-REDBOOKS Entry.'.

      * Prepare the request
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK02Q01.
           MOVE LENGTH OF BAQBASE-RBK02Q01 TO BAQ-REQ-BASE-LENGTH.

      * For this request we want to get all Red Book Inventory
      * and not the inventory for a particular author so we set
      * the Xauthor-existence flag to 0 to tell the BAQHAPI that
      * the optional author parameter is not set.
      *
      * Ever wondered why some generated fields are prefix with 'X'?
      * It is because, as in this case, a clash exists with the
      * languages reserved keyword list. AUTHOR is a COBOL keyword.
           MOVE 0 TO Xauthor-existence of BAQBASE-RBK02Q01.

       CA-020.
      * Call the API
      * Passing the address of the API-INFO structure required for the
      * BAQEXEC call. Section X-EXEC is a reuseable routine that is
      * used for all API calls.
           SET WS-API-INFO TO ADDRESS OF BAQ-API-INFO-RBK02I01.
           PERFORM X-EXEC.

      * Check that the call was successful, if not exit the section
      * Routine X-EXEC has displayed the error responses
           IF BAQ-ERROR THEN
              DISPLAY EIBTRNID ' CA-GET-ALL-REDBOOKS BAQEXEC problem'
              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                       (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

           IF BAQ-WARNING THEN
              DISPLAY EIBTRNID ' CA-GET-ALL-REDBOOKS BAQEXEC problem'
              DISPLAY BAQ-RESP-STATUS-MESSAGE
                       (1:BAQ-RESP-STATUS-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
              GO TO CA-999
           END-IF.

       CA-030.
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

      * The RESTFul API provided a RedbookNotFound response body
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
      * that contains the RBK02P01-responseCode404 data structure.
      * Lets address that and display the returned message which
      * should indicate that there are no Red Books in the repository.
      *
      * The RBK02P01-responseCode404 also contains a dynamic array
      * Data Area of authors Red Books, but for this operation this
      * array is not set
                 SET ADDRESS OF RBK02P01-responseCode404 TO WS-ELEMENT
                 STRING EIBTRNID
                  ' EXEC RESTful EP return HTTP Status Code '
                  WS-STATUS-CODE
                  ' MESSAGE ' Xmessage OF RBK02P01-responseCode404
                      (1:Xmessage-length OF RBK02P01-responseCode404)
                  DELIMITED BY SIZE
                  INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-RESPONSE-MSG
              ELSE
      * 404 was returned but there is no RedbookNotFound response body
                 MOVE BAQ-RESP-STATUS-CODE TO WS-STATUS-CODE
                 STRING EIBTRNID
                   ' EXEC RESTful EP return HTTP Status Code '
                   WS-STATUS-CODE
                   ' NO Response Body'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG

                 PERFORM X-WRITE-RESPONSE-MSG
              END-IF
           END-IF.

       CA-040.
      * Process each returned Redbook. Rather than a single entity in
      * the returned Data Area in this case the OAS response body is an
      * array so rather than a '-existence' flag we have '-num' count
      * that details how many elements exist in the array and we will
      * use X-GET-DATA-AREA-ELEMENT to fetch each one in turn.
           IF BAQ-RESP-STATUS-CODE = 200 THEN
              IF responseCode200-num OF BAQBASE-RBK02P01 > 0 THEN

                 STRING EIBTRNID
                   ' Red Book Inventory'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG
                 PERFORM X-WRITE-RESPONSE-MSG

                 PERFORM CAA-GET-EACH-REDBOOK VARYING WS-INDEX
                    FROM 1 BY 1
                    UNTIL WS-INDEX >
                       responseCode200-num OF BAQBASE-RBK02P01 OR
                       WS-RC = FAILED
              ELSE
                 STRING EIBTRNID
                   ' EXEC RESTful EP - No Red Books returned'
                   DELIMITED BY SIZE
                   INTO WS-DISPLAY-MSG
                 PERFORM X-WRITE-RESPONSE-MSG
              END-IF
           END-IF.

       CA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CA-GET-ALL-REDBOOKS Exit. WS-RC='
                      WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CAA-GET-EACH-REDBOOK
      *
      * Gets each book returned by the remote End Point Service by using
      * BAQGETN (Get Next) and displays the Red Book details.
      *----------------------------------------------------------------*
       CAA-GET-EACH-REDBOOK SECTION.
       CAA-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CAA-GET-EACH-REDBOOK Entry.'.

           MOVE responseCode200-dataarea OF BAQBASE-RBK02P01 TO
                WS-DATA-AREA-NAME.

           MOVE LENGTH OF RBK02P01-responseCode200 TO
                                   WS-ELEMENT-LENGTH.

           PERFORM X-GET-DATA-AREA-ELEMENT.

           IF WS-RC = FAILED THEN GO TO CAA-999.

      * We have fetched the Red Book from the Data Area so set the
      * address of the 01 level data structure.
           SET ADDRESS OF RBK02P01-responseCode200 to WS-ELEMENT.

      * For simplicity lets display the content of the Red Book data
      * structure, but it would be better to return this data from the
      * CICS Tx.
      *
      * Note that optional fields have an '-existence' field to denote
      * if the field exists or not.

           DISPLAY EIBTRNID ' Red Book number ' WS-INDEX.

           STRING EIBTRNID ' Title '
                 Xtitle OF RBK02P01-responseCode200
                      (1:Xtitle-length OF RBK02P01-responseCode200)
             DELIMITED BY SIZE
             INTO WS-DISPLAY-MSG.

           PERFORM X-WRITE-RESPONSE-MSG.

      * Red Books typically have more than one author so these are
      * contained in an array in the OAS Redbook schema and thus
      * returned in a dynamic Data Area.  If the OAS Schema defined a
      * 'maxItems' and this is less than the property
      * 'inlineMaxOccursLimit' set in the Gradle Plugin options.yaml
      * then the array would be inlined in the Red Book data structure.
      * But here we use a dynamic length array again.
           PERFORM CAAA-GET-EACH-AUTHOR VARYING WS-INDEX-2
              FROM 1 BY 1
              UNTIL WS-INDEX-2 >
                 authors-num OF RBK02P01-responseCode200 OR
                 WS-RC = FAILED.

           DISPLAY EIBTRNID ' Status '
                 Xstatus OF RBK02P01-responseCode200
                      (1:Xstatus-length OF RBK02P01-responseCode200).

           IF publicationDate-existence
                 OF RBK02P01-responseCode200 > 0 THEN

               DISPLAY EIBTRNID ' Publication Date '
                  publicationDate2 OF RBK02P01-responseCode200
                 (1:publicationDate2-length OF RBK02P01-responseCode200)
           END-IF.

           DISPLAY EIBTRNID.

       CAA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CAA-GET-EACH-REDBOOK Exit. WS-RC='
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
              DISPLAY EIBTRNID ' CAAA-GET-EACH-AUTHOR Entry.'.

           MOVE authors-dataarea OF RBK02P01-responseCode200 TO
                WS-DATA-AREA-NAME.

           MOVE LENGTH OF RBK02P01-authors TO
                                   WS-ELEMENT-LENGTH.

           PERFORM X-GET-DATA-AREA-ELEMENT.

           IF WS-RC = FAILED THEN GO TO CAAA-999.

      * We have fetched the Author from the Data Area so set the
      * address of the 01 level data structure.
           SET ADDRESS OF RBK02P01-authors to WS-ELEMENT.

           DISPLAY EIBTRNID ' Author ' WS-INDEX-2.

           DISPLAY EIBTRNID ' '
                 authors OF RBK02P01-authors
                      (1:authors-length OF RBK02P01-authors).

       CAAA-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CAAA-GET-EACH-AUTHOR Exit. WS-RC='
                   WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CB-GET-REDBOOK
      *
      * Operation getRedbook
      *
      * Sets the content of the BAQBASE-RBK00Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * RESTful End Point(EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK00P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CB-GET-REDBOOK SECTION.
       CB-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CB-GET-REDBOOK Entry.'.


      * Now its your turn!  We have seen from operation getAllRedbooks
      * how to send a request to a remote end point API using
      * z/OS Connect to handle the JSON to COBOL language structure
      * transformation and to process the response using Data Areas
      * for any returned HTTP response codes and also for dynamic length
      * arrays.
      *
      * For the operation getRedbook you will see in the OAS document
      * redbookapi.yaml that describes the remote endpoint API that
      * a required 'title' property is required and an optional
      *'author'. So this request must provide a title in the
      * data structure BAQBASE-RBK00Q01 and possibly an author.
      *
      * If an author is supplied and title cannot be located in the
      * Red Book inventory then the API will return an array of Redbooks
      * that the supplied author has written.  Please refer to Java
      * endpoint API program class RedbooksResource.java to see the
      * test data used in this simple Redbook Api application.
      *
      * If you want to test out the title not found function with a
      * supplied author then please use the author name 'Lydia Parziale'
      * who has two Red Books in the inventory, and some title that does
      * not exist.
      *
      * The getRedbook opertion defines three HTTP Responses
      * 200-OK
      * 404-NOTFOUND
      * 500-INTERNAL-SERVER-ERROR
      *
      * As for getAllRedbooks we can process the response, however the
      * for this operation RBK00P01 structures will contain the response
      * as generated by the Gradle Plugin.
      *
      * Note that getRedbook returns a single Red Book not an array
      * so tha will be simpler to implement.
      *
      * For the 500 - Internal Server Error response this has been
      * defined as content media type of 'text/plain' rather than the
      * normal 'application/json' so here the remote endpoint may just
      * send a textual string in the event it has some error that it
      * cannot handle.  In this case, as the endpoint response is not
      * a JSON body that we can transform to a COBOL language structure
      * z/OS Connect will place the first 1024 chracters in the
      * BAQ-RESPONSE-AREA field BAQ-RESP-STATUS-MESSAGE.
      *
      * Complete the TODO's below to implement the COBOL code that
      * calls remote endpoint API operation getRedbook.

      * Prepare the request for sending
           SET BAQ-REQ-BASE-ADDRESS TO ADDRESS OF BAQBASE-RBK00Q01.
           MOVE LENGTH OF BAQBASE-RBK00Q01 TO BAQ-REQ-BASE-LENGTH.

      * TODO Set the title and title length in BAQBASE-RBK00Q01

      * TODO Set the author and author-length if you want to test
      *      out the NOTFOUND function. Don't forget to set the
      *      '-existence' flag to 1 if an author is supplied as this
      *      is an optional parameter

       CB-020.
      * TODO Call the API
      *      Passing the address of the operations API-INFO structure

      * TODO Check that BAQEXEC returned BAQ-SUCCESS and exit if not

      * TODO Successful call, address the base structure
      *      BAQBASE-RBK00P01
           DISPLAY EIBTRNID ' TODO'.

       CB-030.
      * TODO Process a 500 response code, in this case the response
      *      will be in BAQ-RESP-STATUS-MSG.
      *
      * Note you will need to alter the Java class method getInventory
      * in class RedbooksResource.java to 'return null;' instead of
      * 'return redbooks;' to test this logic and rebuild the
      * application and redeploy.
           IF BAQ-RESP-STATUS-CODE EQUAL 500 THEN
               DISPLAY EIBTRNID ' TODO'
           END-IF.

       CB-040.
      * TODO Process a 404 response code, in this case the response
      *      will be in data structure RBK00P01-responseCode404
      *      accessed via its Data Area responseCode404-dataarea of
      *      BAQBASE-RBK00P01 using BAQGETN
      *      (Hint reuse routine X-GET-DATA-AREA-ELEMENT).
      *
      *      If authorsBooks-num is > 0 then a dynamic length Data Area
      *      exists of the authors Redbooks use it's Data Area to fetch
      *      each Red Book
           IF BAQ-RESP-STATUS-CODE EQUAL 404 THEN
               DISPLAY EIBTRNID ' TODO'
           END-IF.

       CB-050.
      * TODO Process the returned Red Book, check the
      *      responseCode200-existence is 1 and if so use
      *      responseCode200-dataarea to get the returned Red Book
      *      in to data structure RBK00P01-responseCode200 and
      *      display the content (or better yet return from the Tx)
           IF BAQ-RESP-STATUS-CODE = 200 THEN
               DISPLAY EIBTRNID ' TODO'
           END-IF.

       CB-060.
      * TODO We have processed the 3 possible HTTP Status Codes defined
      *      in the OAS redbookapi.yaml document, but what happens if
      *      the remote endpoint API returned an undefined HTTP status
      *      code, a 409-CONFLICT for example?  In this case
      *      BAQEXEC will return a Completion Code of BAQ-WARNING
      *      with BAQ-ZCON-REASON-CODE set to 2011 if the response is
      *      a text string or 2012 if the response is JSON.  The
      *      response is placed in BAQ-RESP-STATUS-MESSAGE (First 1024
      *      characters).
           DISPLAY EIBTRNID ' TODO'.

       CB-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CB-GET-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * CC-CREATE-REDBOOK
      *
      * Operation createRedbook
      *
      * Sets the content of the BAQBASE-RBK01Q01 Request structure
      * ready for the BAQEXEC Call. The call is then made to the
      * RESTful End Point(EP) via BAQEXEC and the z/OS Connect server.
      *
      * Upon success, the BAQBASE-RBK01P01 structure is returned
      * and dependent of the EP HTTP Status Code a DATA AREA element
      * is got and processed.
      *----------------------------------------------------------------*
       CC-CREATE-REDBOOK SECTION.
       CC-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CC-CREATE-REDBOOK Entry.'.

      * Even less help on this one!  Implement the COBOL code to call
      * operation createRedbook to create a new Red Book.
      * The redbookapi.yaml file describes the createRedbook operation
      * noting the required parameter, the request body and the
      * responses.  The BAQBASE-RBK01Q01 language structure defines
      * the COBOL language structure that has fields for the parameter
      * and the request body. These need to be completed.
      * make the BAQEXEC call and process the response whcich will be
      * either 409-CONFLICT, i.e. the Redbook already exists, or 2XX.
      * Here 2XX is used as a wild card to cover any 2nn HTTP status
      * code returned from the remote endpiont API.  For a create type
      * operation we could resonably expect the HTTP response to be
      * 201-CREATED, but sometimes some implementors choose to use
      * 200-OK, so the OAS document covers this case by using 2XX.
      * the response will be accessed via BAQBASE-RBK01P01.

      * TODO Create a new Red Book.

       CC-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' CC-CREATE-REDBOOK Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-INIT
      *
      * Initialize z/OS Connect call by calling BAQINIT this will
      * acquire a connection to a z/OS Connect server and initialise
      * the BAQ Host API ready for communication.
      *----------------------------------------------------------------*
       X-INIT SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-INIT Entry.'.

      * Initialise the Host API
           IF WS-URIMAP NOT = '        ' THEN
              MOVE BAQZ-SERVER-URIMAP TO BAQ-ZCON-PARM-NAME(1)
              SET BAQ-ZCON-PARM-ADDRESS(1) TO ADDRESS OF WS-URIMAP
              DISPLAY EIBTRNID ' URIMAP=' WS-URIMAP
              MOVE 8 to BAQ-ZCON-PARM-LENGTH(1).

           CALL BAQ-INIT-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY EIBTRNID ' INIT Return Code '
                WS-CC9
              DISPLAY EIBTRNID
                      ' INIT See TDQ BAQH or CICS Trace for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' INIT Completion Code '
                WS-CC9
              DISPLAY EIBTRNID ' INIT Reason Code '
                WS-RC9.

      * Check for bad initialisation
           IF NOT BAQ-SUCCESS THEN
              MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9
              MOVE BAQ-ZCON-REASON-CODE TO WS-RC9
              STRING EIBTRNID
                 ' INIT failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-RESPONSE-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)

              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-INIT Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-EXEC
      *
      * Make the BAQEXEC call
      *----------------------------------------------------------------*
       X-EXEC SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-EXEC Entry.'.

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
              DISPLAY EIBTRNID ' EXEC Completion Code '
                WS-CC9
              DISPLAY EIBTRNID ' EXEC Reason Code '
                WS-RC9
              DISPLAY EIBTRNID ' EXEC HTTP Status Code '
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

              STRING EIBTRNID
                 ' EXEC failed with '
                 WS-FAIL-TYPE
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-RESPONSE-MSG

              DISPLAY EIBTRNID ' ' BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
           ELSE
              IF WS-DEBUG = 1 THEN
                 DISPLAY EIBTRNID ' EXEC Status Code '
                   BAQ-RESP-STATUS-CODE
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-EXEC Exit.'.

           EXIT.

      *----------------------------------------------------------------*
      * X-GET-DATA-AREA-ELEMENT
      *
      * Gets each book returned by the End Point Service by using
      * BAQGETN (Get Next) and displays the book details.
      *----------------------------------------------------------------*
       X-GET-DATA-AREA-ELEMENT SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-GET-DATA-AREA-ELEMENT '
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
              DISPLAY EIBTRNID ' GETN Return Code '
                WS-CC9
              DISPLAY EIBTRNID ' GETN See CICS Trace for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' GETN Completion Code '
                WS-CC9
              DISPLAY EIBTRNID ' GETN Reason Code '
                WS-RC9.

           IF NOT BAQ-SUCCESS THEN
              STRING EIBTRNID
                 ' GETN failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-RESPONSE-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-GET-DATA-AREA-ELEMENT Exit. WS-RC='
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
              DISPLAY EIBTRNID ' X-FREE Entry.'.

           CALL BAQ-FREE-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY EIBTRNID ' FREE Return Code '
                WS-CC9
              DISPLAY EIBTRNID ' FREE See CICS Trace for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' FREE Completion Code '
                WS-CC9
              DISPLAY EIBTRNID ' FREE Reason Code '
                WS-RC9.

      * Check for bad free
           IF NOT BAQ-SUCCESS THEN
              STRING EIBTRNID
                 ' FREE failed'
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-RESPONSE-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-FREE Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * X-TERM
      *
      * Terminates the connection to z/OS Connect using BAQTERM.
      *----------------------------------------------------------------*
       X-TERM SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-TERM Entry.'.

      * Terminate the connection
           CALL BAQ-TERM-NAME USING BY REFERENCE BAQ-ZCONNECT-AREA
                              RETURNING WS-BAQ-RC.

           IF WS-BAQ-RC NOT = 0 THEN
              MOVE FAILED TO WS-RC
              MOVE WS-BAQ-RC TO WS-CC9
              DISPLAY EIBTRNID ' TERM Return Code '
                WS-CC9
              DISPLAY EIBTRNID ' TERM See CICS Trace for details '.

           MOVE BAQ-ZCON-COMPLETION-CODE TO WS-CC9.
           MOVE BAQ-ZCON-REASON-CODE TO WS-RC9.

           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' TERM Completion Code '
                WS-CC9
              DISPLAY EIBTRNID ' TERM Reason Code '
                WS-RC9.

           IF NOT BAQ-SUCCESS THEN
              STRING EIBTRNID
                 ' TERM failed'
                 WS-FAIL-TYPE
                 ' CC=' WS-CC9
                 ' RC=' WS-RC9
                 DELIMITED BY SIZE
                 INTO WS-DISPLAY-MSG

              PERFORM X-WRITE-RESPONSE-MSG

              DISPLAY BAQ-ZCON-RETURN-MESSAGE
                        (1:BAQ-ZCON-RETURN-MESSAGE-LEN)
              MOVE FAILED TO WS-RC
           END-IF.

       X-999.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-TERM Exit. WS-RC=' WS-RC.

           EXIT.

      *----------------------------------------------------------------*
      * Handle CICS abend if not caught by RESP
      *----------------------------------------------------------------*
       X-HANDLE-ABEND SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-HANDLE-ABEND Entry.'.

           STRING 'Tx ' EIBTRNID ' ABENDED.' DELIMITED BY SIZE
               INTO WS-DISPLAY-MSG.

           PERFORM X-WRITE-RESPONSE-MSG.

           DISPLAY EIBTRNID ' FAILED ABENDED'.

      *    Free any resources used by BAQEXEC
           PERFORM X-FREE.

      *    Terminate the BAQHAPI connection
           PERFORM X-TERM.

           IF WS-DEBUG = 1 THEN
              DISPLAY EIBTRNID ' X-HANDLE-ABEND Exit. WS-RC=' WS-RC.

           EXEC CICS RETURN END-EXEC.

      *----------------------------------------------------------------*
      * Write messages to console
      *----------------------------------------------------------------*
       X-WRITE-RESPONSE-MSG SECTION.
       X-010.
           IF WS-DEBUG = 1 THEN
              DISPLAY WS-DISPLAY-MSG.

           IF WS-NETNAME NOT EQUAL ALL '?'
              EXEC CICS SEND TEXT
                   FROM(WS-DISPLAY-MSG)
                   LENGTH(78)
                   TERMINAL
                   ACCUM
                   END-EXEC
           END-IF.

           MOVE SPACES TO WS-DISPLAY-MSG.

           EXIT.