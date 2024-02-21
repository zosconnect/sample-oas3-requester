**********************************************************************
* Copyright IBM Corp. 2023
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
* 
*     http://www.apache.org/licenses/LICENSE-2.0
* 
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
***********************************************************************

DELETE PROGRAM(BAQHRBKC) GROUP(BAQHRBKG)
DEFINE PROGRAM(BAQHRBKC) GROUP(BAQHRBKG)
       DESCRIPTION(API requester COBOL or PLI sample program)

DELETE TRANSACTION(CRBK) GROUP(BAQHRBKG)
DEFINE TRANSACTION(CRBK) GROUP(BAQHRBKG)
       DESCRIPTION(Call the createRedbook operation of the RedbookAPI)
       PROGRAM(BAQHRBKC)

DELETE TRANSACTION(GARB) GROUP(BAQHRBKG)
DEFINE TRANSACTION(GARB) GROUP(BAQHRBKG)
       DESCRIPTION(Call the getRedbooks operation of the RedbookAPI)
       PROGRAM(BAQHRBKC)

DELETE TRANSACTION(GRBK) GROUP(BAQHRBKG)
DEFINE TRANSACTION(GRBK) GROUP(BAQHRBKG)
       DESCRIPTION(Call the getRedbook operation of the RedbookAPI)
       PROGRAM(BAQHRBKC)
