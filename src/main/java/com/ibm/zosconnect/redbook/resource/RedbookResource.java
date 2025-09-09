/*
 * Copyright IBM Corp. 2023, 2025
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
 */
package com.ibm.zosconnect.redbook.resource;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.zosconnect.redbook.model.Author;
import com.ibm.zosconnect.redbook.model.Redbook;
import com.ibm.zosconnect.redbook.model.RedbookNotFound;
import jakarta.json.*;

@Path("/redbook")
public class RedbookResource {


    /**
     * Returns the Redbook for the given title, if the author is supplied and the Redbook cannot be
     * located a list of all the authors works are returned.
     *
     * @param title
     * @param author (optional)
     * @return Redbook
     */
    @GET
    @Path("{title}")
    @Produces("application/json")
    public Response getRedbook(@PathParam("title") String title,
        @QueryParam("author") String author){

        if (author != null) {
            String[] names = author.split(" ");
            Author authorO = new Author(names[0], names[1]);
            Set<Redbook> authorsRedbooks = RedbooksResource.getInventory(authorO);
            if (authorsRedbooks == null || authorsRedbooks.isEmpty()) {
                RedbookNotFound noRedbook = new RedbookNotFound("No Redbooks located for author.");
                System.out.println(noRedbook);
                return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
            }

            List<Redbook> filteredRedbooks  = authorsRedbooks.stream().
                    filter(b -> b.getTitle().equals(title)).
                    collect(Collectors.toList());

            if (filteredRedbooks.size() == 0) {
                RedbookNotFound noRedbook = new RedbookNotFound("No Redbooks located for author, works returned.");
                noRedbook.setAuthorsWorks(new ArrayList<Redbook>(authorsRedbooks));
                System.out.println(noRedbook);
                return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
            }

            System.out.println(filteredRedbooks.get(0).toString());
            return Response.ok().entity(filteredRedbooks.get(0)).build();
        }

        Map<String, Redbook> inventory = RedbooksResource.getInventory();
        if (inventory == null) {
            return Response.status(Status.INTERNAL_SERVER_ERROR).entity("No Redbooks exist in inventory").type("text/plain").build();
        }

        if (inventory.containsKey(title)) {
            System.out.println(inventory.get(title));
            return Response.ok().entity(inventory.get(title)).build();
        }

        RedbookNotFound noRedbook = new RedbookNotFound("Redbook is not located in inventory.");
        System.out.println(noRedbook);
        return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
    }

    /**
     * Creates a new Redbook in inventory.
     *
     * If the title already exists it is returned as a 409 (CONFLICT)
     *
     * @param title
     * @return Redbook
     */
    @POST
    @Path("{title}")
    @Consumes("application/json")
    @Produces("application/json")
    public Response createRedbook(@PathParam("title") String title,
                                  Redbook redbook){

        if (!RedbooksResource.getInventory().containsKey(title)) {
            RedbooksResource.getInventory().put(title, redbook);
            return Response.status(Status.CREATED).entity(RedbooksResource.getInventory().get(title)).build();
        } else {
            return Response.status(Status.CONFLICT).entity(RedbooksResource.getInventory().get(title)).build();
        }
    }

    /**
     * Receives a patch document consistent with RFC 6902, the Redbook is retrieved from
     * inventory and the patch applied to update the Redbook.
     * The updated Redbook is returned.
     *
     * Media-type application/json-patch+json - RFC 6902
     *
     * @param title
     * @param operations
     * @return
     */
    @PATCH
    @Path("{title}")
    @Produces("application/json")
    @Consumes("application/json-patch+json")
    public Response patchRedbook(@PathParam("title") String title,
                                 String operations) throws IOException {

        /*
         * Create the JsonPatch from the passed JSON patch document
         */
        JsonReader jsonReader = Json.createReader(new StringReader(operations));
        JsonPatch patch = Json.createPatch(jsonReader.readArray());
        jsonReader.close();

        if (RedbooksResource.getInventory().containsKey(title)) {
            Redbook redbook = RedbooksResource.getInventory().get(title);

            /*
             * Convert Redbook object to raw JSON
             */
            ObjectMapper objectMapper = new ObjectMapper();
            StringWriter stWriter = new StringWriter();
            objectMapper.writeValue(stWriter, redbook);
            String json = stWriter.toString();
            stWriter.close();

            /*
             * Create a Jakarta JsonObject from the json
             */
            jsonReader = Json.createReader(new StringReader(json));
            JsonObject jsonRedbook = jsonReader.readObject();
            jsonReader.close();

            /*
             * Apply the JSON Patch document to the JsonObject
             */
            JsonObject patchedRedbook = patch.apply(jsonRedbook);

            /*
             * Convert the JsonArray back to JSON then to a Redbook
             */
            StringWriter jsonStWriter = new StringWriter();
            JsonWriter jsonWriter = Json.createWriter(jsonStWriter);
            jsonWriter.writeObject(patchedRedbook);
            jsonWriter.close();
            json = jsonStWriter.toString();
            Redbook updatedRedbook = objectMapper.readValue(json, Redbook.class);

            /*
             * Stored the updated Redbook
             */
            RedbooksResource.getInventory().put(title, updatedRedbook);
            return Response.ok().entity(RedbooksResource.getInventory().get(title)).build();
        }

        RedbookNotFound noRedbook = new RedbookNotFound("Redbook " + title + " is not located in inventory.");
        System.out.println(noRedbook);
        return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
    }

    /**
     * Receives a patch document consistent with RFC 7396, the Redbook is retrieved from
     * inventory and the patch applied to update the Redbook.
     * The updated Redbook is returned.
     *
     * Media-type application/merge-patch+json - RFC 7396
     *
     * @param title
     * @param patchRedbook
     * @return
     */
    @PATCH
    @Path("m/{title}")
    @Produces("application/json")
    @Consumes("application/merge-patch+json")
    public Response mergeRedbook(@PathParam("title") String title,
                                 String patchRedbook) throws IOException {


        /*
         * Create the JsonMergePatch from the passed JSON patch document
         */
        JsonReader jsonReader = Json.createReader(new StringReader(patchRedbook));
        JsonMergePatch patch = Json.createMergePatch(jsonReader.readObject());
        jsonReader.close();

        if (RedbooksResource.getInventory().containsKey(title)) {
            Redbook redbook = RedbooksResource.getInventory().get(title);

            /*
             * Convert Redbook object to raw JSON
             */
            ObjectMapper objectMapper = new ObjectMapper();
            StringWriter stWriter = new StringWriter();
            objectMapper.writeValue(stWriter, redbook);
            String json = stWriter.toString();
            stWriter.close();

            /*
             * Create a Jakarta JsonObject from the json
             */
            jsonReader = Json.createReader(new StringReader(json));
            JsonObject jsonRedbook = jsonReader.readObject();
            jsonReader.close();

            /*
             * Apply the JSON Merge Patch document to the JsonObject
             */
            JsonValue patchedRedbook = patch.apply(jsonRedbook);

            /*
             * Convert the JsonArray back to JSON then to a Redbook
             */
            StringWriter jsonStWriter = new StringWriter();
            JsonWriter jsonWriter = Json.createWriter(jsonStWriter);
            jsonWriter.write(patchedRedbook);
            jsonWriter.close();
            json = jsonStWriter.toString();
            Redbook updatedRedbook = objectMapper.readValue(json, Redbook.class);

            /*
             * Stored the updated book
             */
            RedbooksResource.getInventory().put(title, updatedRedbook);
            return Response.ok().entity(RedbooksResource.getInventory().get(title)).build();
        }

        RedbookNotFound noRedbook = new RedbookNotFound("Redbook " + title + " is not located in inventory.");
        System.out.println(noRedbook);
        return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
    }
}
