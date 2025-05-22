/*
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
 */
package com.ibm.zosconnect.redbook.resource;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.ibm.zosconnect.redbook.model.Redbook;
import com.ibm.zosconnect.redbook.model.RedbookNotFound;

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
            Set<Redbook> authorsRedbooks = RedbooksResource.getInventory(author);
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

        RedbookNotFound noBook = new RedbookNotFound("Redbook is not located in inventory.");
        System.out.println(noBook);
        return Response.status(Status.NOT_FOUND).entity(noBook).build();
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
}
