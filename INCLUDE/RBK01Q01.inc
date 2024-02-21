 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 |    <* This file contains the generated language structure(s) for
 |        request JSON schema 'createRedbook_request.json'.
 |       This structure was generated using 'DFHJS2LS' at mapping
 |        level '5.0'. *>
 |  
 |  Dcl 01 BAQBASE_RBK01Q01,
 |     03 requestPathParameters,
 | 
 |        <* Comments for field 'title':
 |           This field represents the value of JSON schema keyword
 |            'requestPathParameters->title'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '80'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 title                          CHAR(80) VARYING UNALIGNED,
 |     03 requestBody,
 | 
 |        <* Comments for field 'title':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->title'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '80'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 title                          CHAR(80) VARYING UNALIGNED,
 | 
 | 
 |        <* Data area 'authors_dataarea' contains 'authors_num'
 |            instances of structure 'RBK01Q01_authors', each of
 |            which represents an instance of JSON schema keyword
 |            'requestBody->authors'. The Data area must be read
 |            from and written to in BIT mode.
 |           There should be at least '0' instance(s).
 |           There should be at most '20' instance(s). *>
 |       06 authors_num                    UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |       06 authors_dataarea               CHAR(16),
 | 
 | 
 |        <* Comments for field 'status':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->status'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'enumeration' value: '(DRAFT,
 | 
 |            PUBLISHED)'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 status                         CHAR(9) VARYING UNALIGNED,
 | 
 |        <* Comments for field 'formNumber':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->formNumber'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '12'.
 |           JSON schema keyword 'maxLength' value: '12'. *>
 |       06 formNumber                     CHAR(12),
 | 
 | 
 |        <* JSON schema keyword 'requestBody->publicationDate' is
 |            optional. The existence of the field is indicated by
 |            field 'publicationDate_existence'. *>
 |       06 publicationDate_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 | 
 |        <* Comments for field 'publicationDate':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->publicationDate'.
 |           JSON schema type: 'date'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 | 
 |        <* JSON schema keyword 'requestBody->documentType' is
 |            optional. The existence of the field is indicated by
 |            field 'documentType_existence'. *>
 |       06 documentType_existence         UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 | 
 |        <* Comments for field 'documentType':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->documentType'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'enumeration' value: '(PDF,
 | 
 |            HARDCOPY)'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 documentType                   CHAR(8) VARYING UNALIGNED,
 | 
 | 
 |        <* JSON schema keyword 'requestBody->sizeMB' is optional.
 |            The existence of the field is indicated by field
 |            'sizeMB_existence'. *>
 |       06 sizeMB_existence               UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 | 
 |        <* Comments for field 'sizeMB':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->sizeMB'.
 |           JSON schema type: 'number'.
 |           JSON schema keyword 'format' value: 'decimal'.
 |           JSON schema keyword 'minimum' value: '0'. *>
 |       06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 | 
 |        <* JSON schema keyword 'requestBody->url' is optional.
 |            The existence of the field is indicated by field
 |            'url_existence'. *>
 |       06 url_existence                  UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 | 
 |        <* Comments for field 'url':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->url'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '100'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 url                            CHAR(100) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors_dataarea'. *>
 |  Dcl 01 RBK01Q01_authors,
 | 
 |    <* Comments for field 'authors':
 |       This field represents the value of JSON schema keyword
 |        'requestBody->authors'.
 |       JSON schema type: 'string'.
 |       JSON schema keyword 'minLength' value: '0'.
 |       JSON schema keyword 'maxLength' value: '40'.
 |       This field contains a varying length array of characters
 |        or binary data. *>
 |    03 authors                        CHAR(40) VARYING UNALIGNED
 |  
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
 
    Dcl 01 BAQBASE_RBK01Q01,
       03 requestPathParameters,
         06 title                          CHAR(80) VARYING UNALIGNED,
       03 requestBody,
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

         06 url                            CHAR(100) VARYING UNALIGNED;
 
    Dcl 01 RBK01Q01_authors,
      03 authors                        CHAR(40) VARYING UNALIGNED;
 
