 /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 |    <* This file contains the generated language structure(s) for
 |        request JSON schema 'patchRedbook_request.json'.
 |       This structure was generated using 'DFHJS2LS' at mapping
 |        level '5.0'. *>
 |  
 |  Dcl 01 BAQBASE_RBK02Q01,
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
 |        <* Comments for field 'title_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.
 |           A value of 'U' is used to update the JSON property
 |            with the following value, and a value of 'D' is used
 |            to delete the JSON property.  A value of space means
 |            no update is made to the JSON property.
 |           For media-type 'application/json-patch+json', if this
 |            field is part of an array element and every such
 |            matching patch operation field is set to the value
 |            'D' then the complete array element is deleted.
 |           For media-type 'application/merge-patch+json', then
 |            arrays are updated with a complete set of array items
 |            and as such, array items do not include any patch
 |            operation fields.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 title_patch_operation          CHAR(1),
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
 |        <* Comments for field 'authors_patch_item':
 |           This field is used to denote which JSON items are to
 |            be updated during an HTTP PATCH operation. The
 |            following array will contain the array element data
 |            used to update the JSON array items.
 |           The value of the field is a list of array items to
 |            update indexed from 1.  If a new array item is to be
 |            created in the array, the '+' symbol is used to
 |            denote this and must appear last in the list. When a
 |            new array item is created the patch operation field
 |            is ignored. All data fields are used to create a new
 |            JSON array item.
 |           Examples are '4', '1,4,5', '3,5-7', '3-5,+'.  The
 |            value 'DELETE' denotes the whole JSON property of
 |            type array is to be deleted.  The value 'REPLACE'
 |            denotes the whole JSON property of type array is to
 |            be replaced.
 |           A spaces value means no update to be made to the JSON
 |            array.
 |           The arrays '-num' field will contain the number of
 |            items in the array. A correlation exists between the
 |            patch item field and '-num'. For example, i.e. if the
 |            patch item is '3,5-7', then '-num' must be '4'. Each
 |            array element matches the sequence listed in the path
 |            item field.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '50'.
 |           JSON schema keyword 'maxLength' value: '50'. *>
 |       06 authors_patch_item             CHAR(50),
 | 
 | 
 |        <* Data area 'authors_dataarea' contains 'authors_num'
 |            instances of structure 'RBK02Q01_authors', each of
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
 |        <* Comments for field 'status_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 status_patch_operation         CHAR(1),
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
 |        <* Comments for field 'formNumber_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 formNumber_patch_operation     CHAR(1),
 | 
 |        <* Comments for field 'formNumber':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->formNumber'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '12'.
 |           JSON schema keyword 'maxLength' value: '12'. *>
 |       06 formNumber                     CHAR(12),
 | 
 |        <* Comments for field 'blicationDate_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 blicationDate_patch_operation  CHAR(1),
 | 
 |        <* Comments for field 'publicationDate':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->publicationDate'.
 |           JSON schema type: 'date'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 publicationDate                CHAR(32) VARYING UNALIGNED,
 | 
 |        <* Comments for field 'documentType_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 documentType_patch_operation   CHAR(1),
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
 |        <* Comments for field 'sizeMB_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 sizeMB_patch_operation         CHAR(1),
 | 
 |        <* Comments for field 'sizeMB':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->sizeMB'.
 |           JSON schema type: 'number'.
 |           JSON schema keyword 'format' value: 'decimal'.
 |           JSON schema keyword 'minimum' value: '0'. *>
 |       06 sizeMB                         FIXED DECIMAL (31,2),
 | 
 |        <* Comments for field 'url_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 url_patch_operation            CHAR(1),
 | 
 |        <* Comments for field 'url':
 |           This field represents the value of JSON schema keyword
 |            'requestBody->url'.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '0'.
 |           JSON schema keyword 'maxLength' value: '100'.
 |           This field contains a varying length array of
 |            characters or binary data. *>
 |       06 url                            CHAR(100) VARYING UNALIGNED,
 | 
 |        <* Comments for field 'ingDepartment_patch_operation':
 |           This field is used to control how the following field
 |            is used to update a JSON property during an HTTP
 |            PATCH operation.  See the first patch operation field
 |            for a description of use.
 |           JSON schema type: 'string'.
 |           JSON schema keyword 'minLength' value: '1'.
 |           JSON schema keyword 'maxLength' value: '1'. *>
 |       06 ingDepartment_patch_operation  CHAR(1),
 |       06 owningDepartment,
 | 
 |          <* Comments for field 'id_patch_operation':
 |             This field is used to control how the following
 |              field is used to update a JSON property during an
 |              HTTP PATCH operation.  See the first patch
 |              operation field for a description of use.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '1'.
 |             JSON schema keyword 'maxLength' value: '1'. *>
 |         09 id_patch_operation             CHAR(1),
 | 
 |          <* Comments for field 'id':
 |             This field represents the value of JSON schema
 |              keyword 'requestBody->owningDepartment->id'.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '5'.
 |             JSON schema keyword 'maxLength' value: '5'. *>
 |         09 id                             CHAR(5),
 | 
 |          <* Comments for field 'name_patch_operation':
 |             This field is used to control how the following
 |              field is used to update a JSON property during an
 |              HTTP PATCH operation.  See the first patch
 |              operation field for a description of use.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '1'.
 |             JSON schema keyword 'maxLength' value: '1'. *>
 |         09 name_patch_operation           CHAR(1),
 | 
 |          <* Comments for field 'name':
 |             This field represents the value of JSON schema
 |              keyword 'requestBody->owningDepartment->name'.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '0'.
 |             JSON schema keyword 'maxLength' value: '40'.
 |             This field contains a varying length array of
 |              characters or binary data. *>
 |         09 name                           CHAR(40) VARYING
 |     UNALIGNED,
 | 
 |          <* Comments for field 'contact_patch_operation':
 |             This field is used to control how the following
 |              field is used to update a JSON property during an
 |              HTTP PATCH operation.  See the first patch
 |              operation field for a description of use.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '1'.
 |             JSON schema keyword 'maxLength' value: '1'. *>
 |         09 contact_patch_operation        CHAR(1),
 | 
 |          <* Comments for field 'contact':
 |             This field represents the value of JSON schema
 |              keyword 'requestBody->owningDepartment->contact'.
 |             JSON schema type: 'string'.
 |             JSON schema keyword 'minLength' value: '0'.
 |             JSON schema keyword 'maxLength' value: '40'.
 |             This field contains a varying length array of
 |              characters or binary data. *>
 |         09 contact                        CHAR(40) VARYING
 |     UNALIGNED
 |  
 |    <* This structure describes one instance of the data in Data
 |        Area 'authors_dataarea'. *>
 |  Dcl 01 RBK02Q01_authors,
 |    03 authors,
 | 
 |      <* Comments for field 'firstName_patch_operation':
 |         This field is used to control how the following field is
 |          used to update a JSON property during an HTTP PATCH
 |          operation.  See the first patch operation field for a
 |          description of use.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '1'.
 |         JSON schema keyword 'maxLength' value: '1'. *>
 |      06 firstName_patch_operation      CHAR(1),
 | 
 |      <* Comments for field 'firstName':
 |         This field represents the value of JSON schema keyword
 |          'requestBody->authors->firstName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 firstName                      CHAR(40) VARYING UNALIGNED,
 | 
 |      <* Comments for field 'lastName_patch_operation':
 |         This field is used to control how the following field is
 |          used to update a JSON property during an HTTP PATCH
 |          operation.  See the first patch operation field for a
 |          description of use.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '1'.
 |         JSON schema keyword 'maxLength' value: '1'. *>
 |      06 lastName_patch_operation       CHAR(1),
 | 
 |      <* Comments for field 'lastName':
 |         This field represents the value of JSON schema keyword
 |          'requestBody->authors->lastName'.
 |         JSON schema type: 'string'.
 |         JSON schema keyword 'minLength' value: '0'.
 |         JSON schema keyword 'maxLength' value: '40'.
 |         This field contains a varying length array of characters
 |          or binary data. *>
 |      06 lastName                       CHAR(40) VARYING UNALIGNED
 |  
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
 
 
    Dcl 01 BAQBASE_RBK02Q01,
       03 requestPathParameters,
         06 title                          CHAR(80) VARYING UNALIGNED,
       03 requestBody,
         06 title_patch_operation          CHAR(1),
         06 title                          CHAR(80) VARYING UNALIGNED,
         06 authors_patch_item             CHAR(50),

         06 authors_num                    UNSIGNED FIXED BINARY(32)
          UNALIGNED,
         06 authors_dataarea               CHAR(16),

         06 status_patch_operation         CHAR(1),
         06 status                         CHAR(9) VARYING UNALIGNED,
         06 formNumber_patch_operation     CHAR(1),
         06 formNumber                     CHAR(12),
         06 blicationDate_patch_operation  CHAR(1),
         06 publicationDate                CHAR(32) VARYING UNALIGNED,
         06 documentType_patch_operation   CHAR(1),
         06 documentType                   CHAR(8) VARYING UNALIGNED,
         06 sizeMB_patch_operation         CHAR(1),
         06 sizeMB                         FIXED DECIMAL (31,2),
         06 url_patch_operation            CHAR(1),
         06 url                            CHAR(100) VARYING UNALIGNED,
         06 ingDepartment_patch_operation  CHAR(1),
         06 owningDepartment,
           09 id_patch_operation             CHAR(1),
           09 id                             CHAR(5),
           09 name_patch_operation           CHAR(1),
           09 name                           CHAR(40) VARYING
          UNALIGNED,
           09 contact_patch_operation        CHAR(1),
           09 contact                        CHAR(40) VARYING
          UNALIGNED;
 
    Dcl 01 RBK02Q01_authors,
      03 authors,
        06 firstName_patch_operation      CHAR(1),
        06 firstName                      CHAR(40) VARYING UNALIGNED,
        06 lastName_patch_operation       CHAR(1),
        06 lastName                       CHAR(40) VARYING UNALIGNED;
 
