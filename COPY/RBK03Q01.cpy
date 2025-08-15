      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      * This file contains the generated language structure(s) for
      *  request JSON schema 'mergeRedbook_request.json'.
      * This structure was generated using 'DFHJS2LS' at mapping level
      *  '5.0'.
      * 
      *  
      *   01 BAQBASE-RBK03Q01.
      *     03 requestPathParameters.
      * 
      * Comments for field 'Xtitle':
      * This field represents the value of JSON schema keyword
      *  'requestPathParameters->title'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '80'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 Xtitle-length                 PIC S9999 COMP-5 SYNC.
      *       06 Xtitle                        PIC X(80).
      *     03 requestBody.
      * 
      * Comments for field 'title-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation.
      * A value of 'U' is used to update the JSON property with the
      *  following value, and a value of 'D' is used to delete the
      *  JSON property.  A value of space means no update is made to
      *  the JSON property.
      * For media-type 'application/json-patch+json', if this field is
      *  part of an array element and every such matching patch
      *  operation field is set to the value 'D' then the complete
      *  array element is deleted.
      * For media-type 'application/merge-patch+json', then arrays are
      *  updated with a complete set of array items and as such, array
      *  items do not include any patch operation fields.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 title-patch-operation         PIC X(1).
      * 
      * Comments for field 'Xtitle2':
      * This field represents the value of JSON schema keyword
      *  'requestBody->title'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '80'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 Xtitle2-length                PIC S9999 COMP-5 SYNC.
      *       06 Xtitle2                       PIC X(80).
      * 
      * Comments for field 'authors-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 authors-patch-operation       PIC X(1).
      * 
      *  
      * Data area 'authors-dataarea' contains 'authors-num' instances
      *  of structure 'RBK03Q01-authors', each of which represents an
      *  instance of JSON schema keyword 'requestBody->authors'. The
      *  Data area must be read from and written to in BIT mode.
      * There should be at least '0' instance(s).
      * There should be at most '20' instance(s).
      *       06 authors-num                   PIC S9(9) COMP-5 SYNC.
      *       06 authors-dataarea              PIC X(16).
      * 
      *  
      * Comments for field 'status-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 status-patch-operation        PIC X(1).
      * 
      * Comments for field 'Xstatus':
      * This field represents the value of JSON schema keyword
      *  'requestBody->status'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'enumeration' value: '(DRAFT, PUBLISHED)'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 Xstatus-length                PIC S9999 COMP-5 SYNC.
      *       06 Xstatus                       PIC X(9).
      * 
      * Comments for field 'formNumber-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 formNumber-patch-operation    PIC X(1).
      * 
      * Comments for field 'formNumber':
      * This field represents the value of JSON schema keyword
      *  'requestBody->formNumber'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '12'.
      * JSON schema keyword 'maxLength' value: '12'.
      *       06 formNumber                    PIC X(12).
      * 
      * Comments for field 'licationDate-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 licationDate-patch-operation  PIC X(1).
      * 
      * Comments for field 'publicationDate':
      * This field represents the value of JSON schema keyword
      *  'requestBody->publicationDate'.
      * JSON schema type: 'date'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 publicationDate-length        PIC S9999 COMP-5 SYNC.
      *       06 publicationDate               PIC X(32).
      * 
      * Comments for field 'documentType-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 documentType-patch-operation  PIC X(1).
      * 
      * Comments for field 'documentType':
      * This field represents the value of JSON schema keyword
      *  'requestBody->documentType'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'enumeration' value: '(PDF, HARDCOPY)'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 documentType-length           PIC S9999 COMP-5 SYNC.
      *       06 documentType                  PIC X(8).
      * 
      * Comments for field 'sizeMB-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 sizeMB-patch-operation        PIC X(1).
      * 
      * Comments for field 'sizeMB':
      * This field represents the value of JSON schema keyword
      *  'requestBody->sizeMB'.
      * JSON schema type: 'number'.
      * JSON schema keyword 'format' value: 'decimal'.
      * JSON schema keyword 'minimum' value: '0'.
      *       06 sizeMB                        PIC 9(16)V9(2) COMP-3.
      * 
      * Comments for field 'url-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 url-patch-operation           PIC X(1).
      * 
      * Comments for field 'url':
      * This field represents the value of JSON schema keyword
      *  'requestBody->url'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '100'.
      * This field contains a varying length array of characters or
      *  binary data.
      *       06 url-length                    PIC S9999 COMP-5 SYNC.
      *       06 url                           PIC X(100).
      * 
      * Comments for field 'ngDepartment-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *       06 ngDepartment-patch-operation  PIC X(1).
      *       06 owningDepartment.
      * 
      * Comments for field 'id-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *         09 id-patch-operation            PIC X(1).
      * 
      * Comments for field 'Xid':
      * This field represents the value of JSON schema keyword
      *  'requestBody->owningDepartment->id'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '5'.
      * JSON schema keyword 'maxLength' value: '5'.
      *         09 Xid                           PIC X(5).
      * 
      * Comments for field 'name-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *         09 name-patch-operation          PIC X(1).
      * 
      * Comments for field 'name':
      * This field represents the value of JSON schema keyword
      *  'requestBody->owningDepartment->name'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '40'.
      * This field contains a varying length array of characters or
      *  binary data.
      *         09 name-length                   PIC S9999 COMP-5 SYNC.
      *         09 name                          PIC X(40).
      * 
      * Comments for field 'contact-patch-operation':
      * This field is used to control how the following field is used
      *  to update a JSON property during an HTTP PATCH operation. 
      *  See the first patch operation field for a description of use.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '1'.
      * JSON schema keyword 'maxLength' value: '1'.
      *         09 contact-patch-operation       PIC X(1).
      * 
      * Comments for field 'contact':
      * This field represents the value of JSON schema keyword
      *  'requestBody->owningDepartment->contact'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '40'.
      * This field contains a varying length array of characters or
      *  binary data.
      *         09 contact-length                PIC S9999 COMP-5 SYNC.
      *         09 contact                       PIC X(40).
      * 
      *  
      * This structure describes one instance of the data in Data Area
      *  'authors-dataarea'.
      *  01 RBK03Q01-authors.
      *    03 authors.
      * 
      *  
      * JSON schema keyword 'requestBody->authors->firstName' is
      *  optional. The existence of the field is indicated by field
      *  'firstName-existence'.
      *      06 firstName-existence           PIC S9(9) COMP-5 SYNC.
      * 
      *  
      *      06 firstName.
      * 
      * Comments for field 'firstName2':
      * This field represents the value of JSON schema keyword
      *  'requestBody->authors->firstName'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '40'.
      * This field contains a varying length array of characters or
      *  binary data.
      *        09 firstName2-length             PIC S9999 COMP-5 SYNC.
      *        09 firstName2                    PIC X(40).
      * 
      *  
      * JSON schema keyword 'requestBody->authors->lastName' is
      *  optional. The existence of the field is indicated by field
      *  'lastName-existence'.
      *      06 lastName-existence            PIC S9(9) COMP-5 SYNC.
      * 
      *  
      *      06 lastName.
      * 
      * Comments for field 'lastName2':
      * This field represents the value of JSON schema keyword
      *  'requestBody->authors->lastName'.
      * JSON schema type: 'string'.
      * JSON schema keyword 'minLength' value: '0'.
      * JSON schema keyword 'maxLength' value: '40'.
      * This field contains a varying length array of characters or
      *  binary data.
      *        09 lastName2-length              PIC S9999 COMP-5 SYNC.
      *        09 lastName2                     PIC X(40).
      * 
      * Comments for field 'filler':
      * This is a filler entry to ensure the correct padding for a
      *  structure. These slack bytes do not contain any application
      *  data.
      *      06 filler                        PIC X(2).
      * 
      *  
      * ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 
          01 BAQBASE-RBK03Q01.
            03 requestPathParameters.
              06 Xtitle-length                 PIC S9999 COMP-5 SYNC.
              06 Xtitle                        PIC X(80).
            03 requestBody.
              06 title-patch-operation         PIC X(1).
              06 Xtitle2-length                PIC S9999 COMP-5 SYNC.
              06 Xtitle2                       PIC X(80).
              06 authors-patch-operation       PIC X(1).
 
              06 authors-num                   PIC S9(9) COMP-5 SYNC.
              06 authors-dataarea              PIC X(16).
 
              06 status-patch-operation        PIC X(1).
              06 Xstatus-length                PIC S9999 COMP-5 SYNC.
              06 Xstatus                       PIC X(9).
              06 formNumber-patch-operation    PIC X(1).
              06 formNumber                    PIC X(12).
              06 licationDate-patch-operation  PIC X(1).
              06 publicationDate-length        PIC S9999 COMP-5 SYNC.
              06 publicationDate               PIC X(32).
              06 documentType-patch-operation  PIC X(1).
              06 documentType-length           PIC S9999 COMP-5 SYNC.
              06 documentType                  PIC X(8).
              06 sizeMB-patch-operation        PIC X(1).
              06 sizeMB                        PIC 9(16)V9(2) COMP-3.
              06 url-patch-operation           PIC X(1).
              06 url-length                    PIC S9999 COMP-5 SYNC.
              06 url                           PIC X(100).
              06 ngDepartment-patch-operation  PIC X(1).
              06 owningDepartment.
                09 id-patch-operation            PIC X(1).
                09 Xid                           PIC X(5).
                09 name-patch-operation          PIC X(1).
                09 name-length                   PIC S9999 COMP-5 SYNC.
                09 name                          PIC X(40).
                09 contact-patch-operation       PIC X(1).
                09 contact-length                PIC S9999 COMP-5 SYNC.
                09 contact                       PIC X(40).
 
         01 RBK03Q01-authors.
           03 authors.
 
             06 firstName-existence           PIC S9(9) COMP-5 SYNC.
 
             06 firstName.
               09 firstName2-length             PIC S9999 COMP-5 SYNC.
               09 firstName2                    PIC X(40).
 
             06 lastName-existence            PIC S9(9) COMP-5 SYNC.
 
             06 lastName.
               09 lastName2-length              PIC S9999 COMP-5 SYNC.
               09 lastName2                     PIC X(40).
             06 filler                        PIC X(2).
 
