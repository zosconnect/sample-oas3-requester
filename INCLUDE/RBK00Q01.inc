 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 |    <* This file contains the generated language structure(s) for
 |        request JSON schema 'getRedbook_request.json'.
 |       This structure was generated using 'DFHJS2LS' at mapping
 |        level '5.0'. *>
 |  
 |  Dcl 01 BAQBASE_RBK00Q01,
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
 |     03 requestQueryParameters,
 | 
 | 
 |        <* JSON schema keyword 'requestQueryParameters->author'
 |            is optional. The existence of the field is indicated
 |            by field 'author_existence'. *>
 |       06 author_existence               UNSIGNED FIXED BINARY(32)
 |     UNALIGNED,
 | 
 | 
 |        <* Comments for field 'author':
 |           This field represents the value of JSON schema keyword
 |            'requestQueryParameters->author'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '40'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 author                         CHAR(40) VARYING UNALIGNED
 |  
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
 
    Dcl 01 BAQBASE_RBK00Q01,
       03 requestPathParameters,
         06 title                          CHAR(80) VARYING UNALIGNED,
       03 requestQueryParameters,

         06 author_existence               UNSIGNED FIXED BINARY(32)
          UNALIGNED,

         06 author                         CHAR(40) VARYING UNALIGNED;
 
