 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 |    <* This file contains the generated language structure(s) for
 |        response JSON schema 'mergeRedbook_response.json'.
 |       This structure was generated using 'DFHJS2LS' at mapping
 |        level '5.0'. *>
 |  
 |  Dcl BAQBASE_RBK03P01_PTR PTR
 |  Dcl 01 BAQBASE_RBK03P01 BASED(BAQBASE_RBK03P01_PTR),
 | 
 | 
 |      <* Data area 'responseCode200_dataarea' contains
 |          'responseCode200_existence' instances of structure
 |          'RBK03P01_responseCode200', each of which represents an
 |          instance of JSON schema keyword 'responseCode200'. The
 |          Data area must be read from and written to in BIT mode.
 |         
 |         There should be at least '0' instance(s).
 |         There is no maximum number of instances. *>
 |     03 responseCode200_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |     03 responseCode200_dataarea       CHAR(16),
 | 
 | 
 | 
 |      <* Data area 'responseCode404_dataarea' contains
 |          'responseCode404_existence' instances of structure
 |          'RBK03P01_responseCode404', each of which represents an
 |          instance of JSON schema keyword 'responseCode404'. The
 |          Data area must be read from and written to in BIT mode.
 |         
 |         There should be at least '0' instance(s).
 |         There is no maximum number of instances. *>
 |     03 responseCode404_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |     03 responseCode404_dataarea       CHAR(16)
 | 
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'responseCode200_dataarea'. *>
 |  Dcl RBK03P01_responseCode200_PTR PTR
 |  Dcl 01 RBK03P01_responseCode200
 |     BASED(RBK03P01_responseCode200_PTR),
 |    03 responseCode200,
 | 
 |      <* Comments for field 'title':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->title'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '80'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 title                          CHAR(80) VARYING UNALIGNED,
 | 
 |  
 |      <* Data area 'authors_dataarea' contains 'authors_num'
 |          instances of structure 'RBK03P01_authors', each of
 |          which represents an instance of JSON schema keyword
 |          'responseCode200->authors'. The Data area must be read
 |          from and written to in BIT mode.
 |         There should be at least '0' instance(s).
 |         There should be at most '20' instance(s). *>
 |      06 authors_num                    UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |      06 authors_dataarea               CHAR(16),
 | 
 |  
 |      <* Comments for field 'status':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->status'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(DRAFT,
 | 
 |          PUBLISHED)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 status                         CHAR(9) VARYING UNALIGNED,
 | 
 |      <* Comments for field 'formNumber':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->formNumber'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '12'.
 |         JSON schema keyword 'maxLength' value: '12'. *>
 |      06 formNumber                     CHAR(12),
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->publicationDate'
 |          is optional. The existence of the field is indicated by
 |          field 'publicationDate_existence'. *>
 |      06 publicationDate_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'publicationDate':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->publicationDate'.
 |         JSON schema type: 'date'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->documentType' is
 |          optional. The existence of the field is indicated by
 |          field 'documentType_existence'. *>
 |      06 documentType_existence         UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'documentType':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->documentType'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(PDF,
 | 
 |          HARDCOPY)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 documentType                   CHAR(8) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->sizeMB' is
 |          optional. The existence of the field is indicated by
 |          field 'sizeMB_existence'. *>
 |      06 sizeMB_existence               UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'sizeMB':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->sizeMB'.
 |         JSON schema type: 'number'.
 |         JSON schema keyword 'format' value: 'decimal'.
 |         JSON schema keyword 'minimum' value: '0'. *>
 |      06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->url' is optional.
 |          The existence of the field is indicated by field
 |          'url_existence'. *>
 |      06 url_existence                  UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'url':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->url'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '100'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 url                            CHAR(100) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->owningDepartment'
 |          is optional. The existence of the field is indicated by
 |          field 'owningDepartment_existence'. *>
 |      06 owningDepartment_existence     UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      06 owningDepartment,
 | 
 |        <* Comments for field 'id':
 |           This field represents the value of JSON schema keyword
 |            'responseCode200->owningDepartment->id'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '5'.
 |           JSON schema keyword 'maxLength' value: '5'. *>
 |        09 id                             CHAR(5),
 | 
 |        <* Comments for field 'name':
 |           This field represents the value of JSON schema keyword
 |            'responseCode200->owningDepartment->name'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '40'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |        09 name                           CHAR(40) VARYING UNALIGNED,
 | 
 |        <* Comments for field 'contact':
 |           This field represents the value of JSON schema keyword
 |            'responseCode200->owningDepartment->contact'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '40'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |        09 contact                        CHAR(40) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors_dataarea'. *>
 |  Dcl RBK03P01_authors_PTR PTR
 |  Dcl 01 RBK03P01_authors BASED(RBK03P01_authors_PTR),
 |    03 authors,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode200->authors->firstName' is optional. The
 |          existence of the field is indicated by field
 |          'firstName_existence'. *>
 |      06 firstName_existence            UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'firstName':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->authors->firstName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 firstName                      CHAR(40) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode200->authors->lastName'
 |          is optional. The existence of the field is indicated by
 |          field 'lastName_existence'. *>
 |      06 lastName_existence             UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'lastName':
 |         This field represents the value of JSON schema keyword
 |          'responseCode200->authors->lastName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 lastName                       CHAR(40) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'responseCode404_dataarea'. *>
 |  Dcl RBK03P01_responseCode404_PTR PTR
 |  Dcl 01 RBK03P01_responseCode404
 |     BASED(RBK03P01_responseCode404_PTR),
 |    03 responseCode404,
 | 
 |      <* Comments for field 'message':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->message'.
 |         JSON schema type: 'string'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 message                        CHAR(50) VARYING UNALIGNED,
 | 
 |  
 |      <* Data area 'authorsBooks_dataarea' contains
 |          'authorsBooks_num' instances of structure
 |          'RBK03P01_authorsBooks', each of which represents an
 |          instance of JSON schema keyword
 |          'responseCode404->authorsBooks'. The Data area must be
 |          read from and written to in BIT mode.
 |         There should be at least '0' instance(s).
 |         There is no maximum number of instances. *>
 |      06 authorsBooks_num               UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |      06 authorsBooks_dataarea          CHAR(16)
 |  
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authorsBooks_dataarea'. *>
 |  Dcl RBK03P01_authorsBooks_PTR PTR
 |  Dcl 01 RBK03P01_authorsBooks BASED(RBK03P01_authorsBooks_PTR),
 |    03 authorsBooks,
 | 
 |      <* Comments for field 'title':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->title'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '80'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 title                          CHAR(80) VARYING UNALIGNED,
 | 
 |  
 |      <* Data area 'authors3_dataarea' contains 'authors3_num'
 |          instances of structure 'RBK03P01_authors3', each of
 |          which represents an instance of JSON schema keyword
 |          'responseCode404->authorsBooks->authors'. The Data area
 |          must be read from and written to in BIT mode.
 |         There should be at least '0' instance(s).
 |         There should be at most '20' instance(s). *>
 |      06 authors3_num                   UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |      06 authors3_dataarea              CHAR(16),
 | 
 |  
 |      <* Comments for field 'status':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->status'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(DRAFT,
 | 
 |          PUBLISHED)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 status                         CHAR(9) VARYING UNALIGNED,
 | 
 |      <* Comments for field 'formNumber':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->formNumber'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '12'.
 |         JSON schema keyword 'maxLength' value: '12'. *>
 |      06 formNumber                     CHAR(12),
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->publicationDate' is
 |          optional. The existence of the field is indicated by
 |          field 'publicationDate2_existence'. *>
 |      06 publicationDate2_existence     UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'publicationDate':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->publicationDate'.
 |         JSON schema type: 'date'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->documentType' is
 |          optional. The existence of the field is indicated by
 |          field 'documentType2_existence'. *>
 |      06 documentType2_existence        UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'documentType':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->documentType'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(PDF,
 | 
 |          HARDCOPY)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 documentType                   CHAR(8) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->sizeMB' is optional.
 |          The existence of the field is indicated by field
 |          'sizeMB2_existence'. *>
 |      06 sizeMB2_existence              UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'sizeMB':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->sizeMB'.
 |         JSON schema type: 'number'.
 |         JSON schema keyword 'format' value: 'decimal'.
 |         JSON schema keyword 'minimum' value: '0'. *>
 |      06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 |  
 |      <* JSON schema keyword 'responseCode404->authorsBooks->url'
 |          is optional. The existence of the field is indicated by
 |          field 'url2_existence'. *>
 |      06 url2_existence                 UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'url':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->url'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '100'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 url                            CHAR(100) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->owningDepartment' is
 |          optional. The existence of the field is indicated by
 |          field 'owningDepartment_existence'. *>
 |      06 owningDepartment_existence     UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      06 owningDepartment3,
 | 
 |        <* Comments for field 'id':
 |           This field represents the value of JSON schema keyword
 |            'responseCode404->authorsBooks->owningDepartment->id'.
 |           
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '5'.
 |           JSON schema keyword 'maxLength' value: '5'. *>
 |        09 id                             CHAR(5),
 | 
 |        <* Comments for field 'name':
 |           This field represents the value of JSON schema keyword
 |            'responseCode404->authorsBooks->owningDepartment->name
 |           '.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '40'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |        09 name                           CHAR(40) VARYING UNALIGNED,
 | 
 |        <* Comments for field 'contact':
 |           This field represents the value of JSON schema keyword
 |            'responseCode404->authorsBooks->owningDepartment->cont
 |           act'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '40'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |        09 contact                        CHAR(40) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors3_dataarea'. *>
 |  Dcl RBK03P01_authors3_PTR PTR
 |  Dcl 01 RBK03P01_authors3 BASED(RBK03P01_authors3_PTR),
 |    03 authors3,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->authors->firstName' is
 |          optional. The existence of the field is indicated by
 |          field 'firstName2_existence'. *>
 |      06 firstName2_existence           UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'firstName':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->authors->firstName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 firstName                      CHAR(40) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword
 |          'responseCode404->authorsBooks->authors->lastName' is
 |          optional. The existence of the field is indicated by
 |          field 'lastName2_existence'. *>
 |      06 lastName2_existence            UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'lastName':
 |         This field represents the value of JSON schema keyword
 |          'responseCode404->authorsBooks->authors->lastName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 lastName                       CHAR(40) VARYING UNALIGNED
 |  
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
 
    Dcl BAQBASE_RBK03P01_PTR PTR;
    Dcl 01 BAQBASE_RBK03P01 BASED(BAQBASE_RBK03P01_PTR),

       03 responseCode200_existence      UNSIGNED FIXED BINARY(32)
          UNALIGNED,
       03 responseCode200_dataarea       CHAR(16),


       03 responseCode404_existence      UNSIGNED FIXED BINARY(32)
          UNALIGNED,
       03 responseCode404_dataarea       CHAR(16);

 
    Dcl RBK03P01_responseCode200_PTR PTR;
    Dcl 01 RBK03P01_responseCode200
          BASED(RBK03P01_responseCode200_PTR),
      03 responseCode200,
        06 title                          CHAR(80) VARYING UNALIGNED,
 
        06 authors_num                    UNSIGNED FIXED BINARY(32)
          UNALIGNED,
        06 authors_dataarea               CHAR(16),
 
        06 status                         CHAR(9) VARYING UNALIGNED,
        06 formNumber                     CHAR(12),
 
        06 publicationDate_existence      UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 publicationDate                CHAR(32) VARYING UNALIGNED,
 
        06 documentType_existence         UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 documentType                   CHAR(8) VARYING UNALIGNED,
 
        06 sizeMB_existence               UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 sizeMB                         FIXED DECIMAL (31,2),
 
        06 url_existence                  UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 url                            CHAR(100) VARYING UNALIGNED,
 
        06 owningDepartment_existence     UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 owningDepartment,
          09 id                             CHAR(5),
          09 name                           CHAR(40) VARYING UNALIGNED,
          09 contact                        CHAR(40) VARYING UNALIGNED;
 
    Dcl RBK03P01_authors_PTR PTR;
    Dcl 01 RBK03P01_authors BASED(RBK03P01_authors_PTR),
      03 authors,
 
        06 firstName_existence            UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 firstName                      CHAR(40) VARYING UNALIGNED,
 
        06 lastName_existence             UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 lastName                       CHAR(40) VARYING UNALIGNED;
 
    Dcl RBK03P01_responseCode404_PTR PTR;
    Dcl 01 RBK03P01_responseCode404
          BASED(RBK03P01_responseCode404_PTR),
      03 responseCode404,
        06 message                        CHAR(50) VARYING UNALIGNED,
 
        06 authorsBooks_num               UNSIGNED FIXED BINARY(32)
          UNALIGNED,
        06 authorsBooks_dataarea          CHAR(16);
 
 
    Dcl RBK03P01_authorsBooks_PTR PTR;
    Dcl 01 RBK03P01_authorsBooks BASED(RBK03P01_authorsBooks_PTR),
      03 authorsBooks,
        06 title                          CHAR(80) VARYING UNALIGNED,
 
        06 authors3_num                   UNSIGNED FIXED BINARY(32)
          UNALIGNED,
        06 authors3_dataarea              CHAR(16),
 
        06 status                         CHAR(9) VARYING UNALIGNED,
        06 formNumber                     CHAR(12),
 
        06 publicationDate2_existence     UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 publicationDate                CHAR(32) VARYING UNALIGNED,
 
        06 documentType2_existence        UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 documentType                   CHAR(8) VARYING UNALIGNED,
 
        06 sizeMB2_existence              UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 sizeMB                         FIXED DECIMAL (31,2),
 
        06 url2_existence                 UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 url                            CHAR(100) VARYING UNALIGNED,
 
        06 owningDepartment_existence     UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 owningDepartment3,
          09 id                             CHAR(5),
          09 name                           CHAR(40) VARYING UNALIGNED,
          09 contact                        CHAR(40) VARYING UNALIGNED;
 
    Dcl RBK03P01_authors3_PTR PTR;
    Dcl 01 RBK03P01_authors3 BASED(RBK03P01_authors3_PTR),
      03 authors3,
 
        06 firstName2_existence           UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 firstName                      CHAR(40) VARYING UNALIGNED,
 
        06 lastName2_existence            UNSIGNED FIXED BINARY(32)
          UNALIGNED,
 
        06 lastName                       CHAR(40) VARYING UNALIGNED;
 
