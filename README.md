# OpenAPI 3 API requester Sample
A sample z/OS Connect API requester project showing how to call an OAS3 defined API from a CICS COBOL application.

## Introduction
This Redbook API requester sample implements a very simple IBM Redbook information application written in Java and hosted in Liberty. This application implements an OpenApi 3.0 described API. See the Sub Gradle project `RedbookApi` redbookapi.yaml file for a full description of the operations.

This project's gradle build will build the remote endpoint RedbookAPI application war file and also generate the z/OS Connect API requester war file.

## Instructions
Follow the instructions in the [IBM Documentation](https://www.ibm.com/docs/en/zos-connect/zos-connect/3.0?topic=gst-creating-cics-cobol-zos-connect-api-requester-application) for details on how to build, deploy and run this tutorial sample.

## Structure
* The `COBOL`	directory contains the CICS COBOL program BAQHRBKC.
* The `CSD` directory contains the CICS CSD file BAQHRBKD for the COBOL program.
* The `deploy/api` directory contains	the sample server.xml for a Liberty server installed with z/OS Connect. This runs the z/OS Connect API requester WAR and the Redbook endpoint application WAR.
* The `RedbookApi` directory contains the API requester Gradle sub-project, called from the outer Gradle build file.
* The `RedbookApi/src/main/api` directory contains the OpenAPI document, redbookapi.yaml, that describes the endpoint API application.
* The `RedbookApi/src/main/config` directory contains	the options.yaml file used to control how the redbookapi.yaml file is processed to produce a z/OS Connect API requester WAR file.
* The `src/main/java/com/ibm/zosconnect/redbook/app` directory contains the RedbookApp Java class that implements a JAX-RS Application for the endpoint application.
* The `src/main/java/com/ibm/zosconnect/redbook/model` directory contains the Java classes that represent the OAS schemas from redbookapi.yaml.
* The `src/main/java/com/ibm/zosconnect/redbook/resource` directory contains the Java classes used to implement the operations defined in redbookapi.yaml.
* The `src/main/webapp/WEB-INF` directory contains the web.xml file required for the Redbook endpoint application.

## RedbookAPI operations
### getAllRedbooks
Path **/redbooks**. Method **GET**. Passes an optional author query parameter of type string and expects an array of Redbook objects for a **200** response.

### getRedbook
Path **/redbook/{bookName}**. Method **GET**. Passes a title path parameter, and optional author query parameter and optional documentType header parameter. A Redbook object is returned for a **200** response. For a **404 (Not Found)** response a RedbookNotFound object is returned containing a message. A **500** for media type **text/plain** with a string schema.

### createRedbook
Path **/redbook/{bookName}**. Method **POST**. Passes a bookName path parameter and a Book object body. Expects a Book object for a **2XX** wildcarded response. A **409 (Conflict)** can also be returned. No other default responses are defined.

## Application Component Schemas
### Redbooks
An array schema type with items as the Redbook schema.

### Redbook
An object schema with properties relating to IBM Red Book metadata.

### RedbookNotFound
A simple object schema with a String message used to denote a Redbook is not found and an array of Redbook objects that the supplied author has written.

## CICS Resources
### BAQHRBKC COBOL program
The COBOL program BAQHRBKC is called for three transactions that then call the three different operations on the remote API endpoint.

### CICS CSD BAQHRBKD
A CSD update file to define the Transactions and Program definitions into group BAQHRBKG.

## License
See `LICENSE` for details.