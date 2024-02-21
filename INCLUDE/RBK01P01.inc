 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 |    <* This file contains the generated language structure(s) for
 |        response JSON schema 'createRedbook_response.json'.
 |       This structure was generated using 'DFHJS2LS' at mapping
 |        level '5.0'. *>
 |  
 |  Dcl BAQBASE_RBK01P01_PTR PTR
 |  Dcl 01 BAQBASE_RBK01P01 BASED(BAQBASE_RBK01P01_PTR),
 | 
 | 
 |      <* Data area 'responseCode2XX_dataarea' contains
 |          'responseCode2XX_existence' instances of structure
 |          'RBK01P01_responseCode2XX', each of which represents an
 |          instance of JSON schema keyword 'responseCode2XX'. The
 |          Data area must be read from and written to in BIT mode.
 |         
 |         There should be at least '0' instance(s).
 |         There is no maximum number of instances. *>
 |     03 responseCode2XX_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |     03 responseCode2XX_dataarea       CHAR(16),
 | 
 | 
 | 
 |      <* Data area 'responseCode409_dataarea' contains
 |          'responseCode409_existence' instances of structure
 |          'RBK01P01_responseCode409', each of which represents an
 |          instance of JSON schema keyword 'responseCode409'. The
 |          Data area must be read from and written to in BIT mode.
 |         
 |         There should be at least '0' instance(s).
 |         There is no maximum number of instances. *>
 |     03 responseCode409_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |     03 responseCode409_dataarea       CHAR(16)
 | 
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'responseCode2XX_dataarea'. *>
 |  Dcl RBK01P01_responseCode2XX_PTR PTR
 |  Dcl 01 RBK01P01_responseCode2XX
 |     BASED(RBK01P01_responseCode2XX_PTR),
 |    03 responseCode2XX,
 | 
 |      <* Comments for field 'title':
 |         This field represents the value of JSON schema keyword
 |          'responseCode2XX->title'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '80'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 title                          CHAR(80) VARYING UNALIGNED,
 | 
 |  
 |      <* Data area 'authors_dataarea' contains 'authors_num'
 |          instances of structure 'RBK01P01_authors', each of
 |          which represents an instance of JSON schema keyword
 |          'responseCode2XX->authors'. The Data area must be read
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
 |          'responseCode2XX->status'.
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
 |          'responseCode2XX->formNumber'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '12'.
 |         JSON schema keyword 'maxLength' value: '12'. *>
 |      06 formNumber                     CHAR(12),
 | 
 |  
 |      <* JSON schema keyword 'responseCode2XX->publicationDate'
 |          is optional. The existence of the field is indicated by
 |          field 'publicationDate_existence'. *>
 |      06 publicationDate_existence      UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'publicationDate':
 |         This field represents the value of JSON schema keyword
 |          'responseCode2XX->publicationDate'.
 |         JSON schema type: 'date'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode2XX->documentType' is
 |          optional. The existence of the field is indicated by
 |          field 'documentType_existence'. *>
 |      06 documentType_existence         UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'documentType':
 |         This field represents the value of JSON schema keyword
 |          'responseCode2XX->documentType'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(PDF,
 | 
 |          HARDCOPY)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 documentType                   CHAR(8) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode2XX->sizeMB' is
 |          optional. The existence of the field is indicated by
 |          field 'sizeMB_existence'. *>
 |      06 sizeMB_existence               UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'sizeMB':
 |         This field represents the value of JSON schema keyword
 |          'responseCode2XX->sizeMB'.
 |         JSON schema type: 'number'.
 |         JSON schema keyword 'format' value: 'decimal'.
 |         JSON schema keyword 'minimum' value: '0'. *>
 |      06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 |  
 |      <* JSON schema keyword 'responseCode2XX->url' is optional.
 |          The existence of the field is indicated by field
 |          'url_existence'. *>
 |      06 url_existence                  UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'url':
 |         This field represents the value of JSON schema keyword
 |          'responseCode2XX->url'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '100'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 url                            CHAR(100) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors_dataarea'. *>
 |  Dcl RBK01P01_authors_PTR PTR
 |  Dcl 01 RBK01P01_authors BASED(RBK01P01_authors_PTR),
 | 
 |    <* Comments for field 'authors':
 |       This field represents the value of JSON schema keyword
 |        'responseCode2XX->authors'.
 |       JSON schema type: 'string'.
 |       JSON schema keyword 'minLength' value: '0'.
 |       JSON schema keyword 'maxLength' value: '40'.
 |       This field contains a varying length array of characters
 |        or binary data. *>
 |    03 authors                        CHAR(40) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'responseCode409_dataarea'. *>
 |  Dcl RBK01P01_responseCode409_PTR PTR
 |  Dcl 01 RBK01P01_responseCode409
 |     BASED(RBK01P01_responseCode409_PTR),
 |    03 responseCode409,
 | 
 |      <* Comments for field 'title':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->title'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '80'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 title                          CHAR(80) VARYING UNALIGNED,
 | 
 |  
 |      <* Data area 'authors2_dataarea' contains 'authors2_num'
 |          instances of structure 'RBK01P01_authors2', each of
 |          which represents an instance of JSON schema keyword
 |          'responseCode409->authors'. The Data area must be read
 |          from and written to in BIT mode.
 |         There should be at least '0' instance(s).
 |         There should be at most '20' instance(s). *>
 |      06 authors2_num                   UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 |      06 authors2_dataarea              CHAR(16),
 | 
 |  
 |      <* Comments for field 'status':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->status'.
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
 |          'responseCode409->formNumber'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '12'.
 |         JSON schema keyword 'maxLength' value: '12'. *>
 |      06 formNumber                     CHAR(12),
 | 
 |  
 |      <* JSON schema keyword 'responseCode409->publicationDate'
 |          is optional. The existence of the field is indicated by
 |          field 'publicationDate2_existence'. *>
 |      06 publicationDate2_existence     UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'publicationDate':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->publicationDate'.
 |         JSON schema type: 'date'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode409->documentType' is
 |          optional. The existence of the field is indicated by
 |          field 'documentType2_existence'. *>
 |      06 documentType2_existence        UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'documentType':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->documentType'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'enumeration' value: '(PDF,
 | 
 |          HARDCOPY)'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 documentType                   CHAR(8) VARYING UNALIGNED,
 | 
 |  
 |      <* JSON schema keyword 'responseCode409->sizeMB' is
 |          optional. The existence of the field is indicated by
 |          field 'sizeMB2_existence'. *>
 |      06 sizeMB2_existence              UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'sizeMB':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->sizeMB'.
 |         JSON schema type: 'number'.
 |         JSON schema keyword 'format' value: 'decimal'.
 |         JSON schema keyword 'minimum' value: '0'. *>
 |      06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 |  
 |      <* JSON schema keyword 'responseCode409->url' is optional.
 |          The existence of the field is indicated by field
 |          'url2_existence'. *>
 |      06 url2_existence                 UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 |  
 |      <* Comments for field 'url':
 |         This field represents the value of JSON schema keyword
 |          'responseCode409->url'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '100'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 url                            CHAR(100) VARYING UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors2_dataarea'. *>
 |  Dcl RBK01P01_authors2_PTR PTR
 |  Dcl 01 RBK01P01_authors2 BASED(RBK01P01_authors2_PTR),
 | 
 |    <* Comments for field 'authors':
 |       This field represents the value of JSON schema keyword
 |        'responseCode409->authors'.
 |       JSON schema type: 'string'.
 |       JSON schema keyword 'minLength' value: '0'.
 |       JSON schema keyword 'maxLength' value: '40'.
 |       This field contains a varying length array of characters
 |        or binary data. *>
 |    03 authors                        CHAR(40) VARYING UNALIGNED
 |  
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
 
    Dcl BAQBASE_RBK01P01_PTR PTR;
    Dcl 01 BAQBASE_RBK01P01 BASED(BAQBASE_RBK01P01_PTR),

       03 responseCode2XX_existence      UNSIGNED FIXED BINARY(32)
          UNALIGNED,
       03 responseCode2XX_dataarea       CHAR(16),


       03 responseCode409_existence      UNSIGNED FIXED BINARY(32)
          UNALIGNED,
       03 responseCode409_dataarea       CHAR(16);

 
    Dcl RBK01P01_responseCode2XX_PTR PTR;
    Dcl 01 RBK01P01_responseCode2XX
          BASED(RBK01P01_responseCode2XX_PTR),
      03 responseCode2XX,
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
 
    Dcl RBK01P01_authors_PTR PTR;
    Dcl 01 RBK01P01_authors BASED(RBK01P01_authors_PTR),
      03 authors                        CHAR(40) VARYING UNALIGNED;
 
    Dcl RBK01P01_responseCode409_PTR PTR;
    Dcl 01 RBK01P01_responseCode409
          BASED(RBK01P01_responseCode409_PTR),
      03 responseCode409,
        06 title                          CHAR(80) VARYING UNALIGNED,
 
        06 authors2_num                   UNSIGNED FIXED BINARY(32)
          UNALIGNED,
        06 authors2_dataarea              CHAR(16),
 
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
 
        06 url                            CHAR(100) VARYING UNALIGNED;
 
    Dcl RBK01P01_authors2_PTR PTR;
    Dcl 01 RBK01P01_authors2 BASED(RBK01P01_authors2_PTR),
      03 authors                        CHAR(40) VARYING UNALIGNED;
 
