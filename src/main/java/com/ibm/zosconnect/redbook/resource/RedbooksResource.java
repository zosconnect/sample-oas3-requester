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
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.QueryParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import com.ibm.zosconnect.redbook.model.Author;
import com.ibm.zosconnect.redbook.model.OwningDepartment;
import com.ibm.zosconnect.redbook.model.Redbook;
import com.ibm.zosconnect.redbook.model.RedbookNotFound;

@Path("/redbooks")
public class RedbooksResource {
    /**
     * When constructed a Map of authors is constructed from the main inventory with an
     * associated Set of Redbooks that the author has published.
     */
    public RedbooksResource() {
        for (Redbook redbook : redbooks.values()) {
            List<Author> authors = redbook.getAuthors();
            for (Author author: authors) {
                if (authorsBooks.containsKey(author)) {
                    Set<Redbook> books = authorsBooks.get(author);
                    books.add(redbook);
                }
                else {
                    Set<Redbook> books = new HashSet<>();
                    books.add(redbook);
                    authorsBooks.put(author, books);
                }
            }
        }
    }

    /**
     * Gets the main inventory of all Redbooks
     * @return
     */
    public static Map<String, Redbook> getInventory() {
        return redbooks;
    }

    /**
     * Gets the inventory of all Redbooks written by a particular author
     * @return
     */
    public static Set<Redbook> getInventory(Author author) {
        return authorsBooks.get(author);
    }

    /**
     * Returns a list of all the Redbooks or all the Redbooks for a particular author.
     *
     * A 404 (NOTFOUND) is returned if non exist.
     *
     * @param author
     * @return
     */
    @GET
    @Produces("application/json")
    public Response getAllRedbooks(@QueryParam("author") String author) {
        if (author == null || author.equals("")) {
            List<Redbook> redbookList = new ArrayList<Redbook>(redbooks.values());
            return Response.status(Status.OK).entity(redbookList).build();
        }

        Set<Redbook> redbookSet = authorsBooks.get(author);
        if (redbookSet == null || redbookSet.isEmpty()) {
            RedbookNotFound noRedbook = new RedbookNotFound("No Redbooks located for author.");
            System.out.println(noRedbook);
            return Response.status(Status.NOT_FOUND).entity(noRedbook).build();
        }

        return Response.status(Status.OK).entity(new ArrayList<Redbook>(redbookSet)).build();
    }


    /*
     * Static collection setup
     */

    private static Map<String, Redbook> redbooks = new LinkedHashMap<>();
    {
        List<Author> authors = new ArrayList<>();
        authors.add(new Author("Lydia", "Parziale"));
        authors.add(new Author("Luiz", "Fadel"));
        authors.add(new Author("Stanley", "Jon"));
        OwningDepartment dept = new OwningDepartment("1", "zOS", "A Manager");
        Redbook redbook = new Redbook("ABCs of IBM zOS System Programming Volume 1", authors, "SG24-6981-04", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 1 - 1, 22).getTime());
        redbook.setSizeMB(4.1);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg246981.pdf");
        redbooks.put("ABCs of IBM zOS System Programming Volume 1", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Lydia", "Parziale"));
        authors.add(new Author("Guillermo", "Cosimo"));
        authors.add(new Author("Lutz", "Kuehner"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("ABCs of IBM zOS System Programming Volume 2", authors, "SG24-6982-04", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 4 - 1, 7).getTime());
        redbook.setSizeMB(3.2);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg246982.pdf");
        redbooks.put("ABCs of IBM zOS System Programming Volume 2", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Jose Gilberto", "Biondo Jr"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("ABCs of IBM zOS System Programming Volume 3", authors, "SG24-6983-04", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 1 - 1, 19).getTime());
        redbook.setSizeMB(2.8);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg246983.pdf");
        redbooks.put("ABCs of IBM zOS System Programming Volume 3", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Makenzie", "Mannaksu"));
        authors.add(new Author("Diego", "Cardalliaguet"));
        authors.add(new Author("Mehmet", "Cuneyt-Goksu"));
        authors.add(new Author("Alex", "Osadchyy"));
        authors.add(new Author("Lih-M", "Wang"));
        authors.add(new Author("Sherry", "Yu"));
        authors.add(new Author("Poonam", "Zham"));
        authors.add(new Author("Erica", "Ross"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("What AI Can Do for You: Use Cases for AI on IBM Z", authors, "REDP-5679-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2022, 8 - 1, 2).getTime());
        redbook.setSizeMB(9.3);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redpapers/pdfs/redp5679.pdf");
        redbooks.put("What AI Can Do for You: Use Cases for AI on IBM Z", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Bill", "White"));
        authors.add(new Author("Matthias", "Bangert"));
        authors.add(new Author("Cyril", "Armand"));
        authors.add(new Author("Roger", "Bales"));
        authors.add(new Author("Diego", "Bessone"));
        authors.add(new Author("Anthony", "Ciabattoni"));
        authors.add(new Author("Michael", "Frankenberg"));
        authors.add(new Author("Debra", "Hallen"));
        authors.add(new Author("DeWayne", "Hughes"));
        authors.add(new Author("Vinod", "Kanwal"));
        authors.add(new Author("Karen", "Smolar"));
        authors.add(new Author("Jean-Marc", "Vandon"));
        authors.add(new Author("Paolo", "Vitali"));
        authors.add(new Author("Knud", "Vraa"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("Getting Started with IBM Z Cyber Vault", authors, "SG24-8511-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2021, 11 - 1, 18).getTime());
        redbook.setSizeMB(5.3);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg248511.pdf");
        redbooks.put("Getting Started with IBM Z Cyber Vault", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Makenzie", "Manna"));
        authors.add(new Author("Ravinder", "Akula"));
        authors.add(new Author("Matthew", "Cousens"));
        authors.add(new Author("Pabitra", "Mukhopadhyay"));
        authors.add(new Author("Anand", "Shukla"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("Getting Started: Journey to Modernization with IBM Z", authors, "REDP-5627-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2021, 3 - 1, 15).getTime());
        redbook.setSizeMB(5.6);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redpapers/pdfs/redp5627.pdf");
        redbooks.put("Getting Started: Journey to Modernization with IBM Z", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Phil", "Wakelin"));
        authors.add(new Author("Carlos", "Donatucci"));
        authors.add(new Author("Jonathan", "Lawrence"));
        authors.add(new Author("Mitch", "Johnson"));
        authors.add(new Author("Michael", "Jones"));
        authors.add(new Author("Tito", "Paiva"));
        dept = new OwningDepartment("2", "CICS", "A Manager");
        redbook = new Redbook("Liberty in IBM CICS: Deploying and Managing Java EE Applications", authors, "SG24-8418-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 3 - 1, 29).getTime());
        redbook.setSizeMB(11.1);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg248418.pdf");
        redbooks.put("Liberty in IBM CICS: Deploying and Managing Java EE Applications", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Chris", "Crone"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("Exploring IBM Db2 for zOS Continuous Delivery", authors, "REDP-5469-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 3 - 1, 21).getTime());
        redbook.setSizeMB(2.8);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redpapers/pdfs/redp5469.pdf");
        redbooks.put("Exploring IBM Db2 for zOS Continuous Delivery", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Arndt", "Eade"));
        authors.add(new Author("Randy", "Frerking"));
        authors.add(new Author("Rich", "Jacksons"));
        authors.add(new Author("Kellie", "Mathis"));
        dept = new OwningDepartment("2", "CICS", "A Manager");
        redbook = new Redbook("IBM CICS and the Coupling Facility: Beyond the Basics", authors, "SG24-8420-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2018, 2 - 1, 21).getTime());
        redbook.setSizeMB(4.6);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg248420.pdf");
        redbooks.put("IBM CICS and the Coupling Facility: Beyond the Basics", redbook);

        authors = new ArrayList<>();
        authors.add(new Author("Keith", "Winnard"));
        authors.add(new Author("Wai", "Choi"));
        authors.add(new Author("Martina", "vondem-Bussche"));
        dept = new OwningDepartment("1", "zOS", "A Manager");
        redbook = new Redbook("zOS PKI Services: Quick Set-up for Multiple CAs", authors, "SG24-8337-00", "PDF", dept);
        redbook.setPublicationDate(new GregorianCalendar(2017, 1 - 1, 20).getTime());
        redbook.setSizeMB(3.3);
        redbook.setStatus(Redbook.Status.PUBLISHED);
        redbook.setUrl("https://www.redbooks.ibm.com/redbooks/pdfs/sg248337.pdf");
        redbooks.put("zOS PKI Services: Quick Set-up for Multiple CAs", redbook);
    }

    private static Map<Author, Set<Redbook>> authorsBooks = new HashMap<>();
}
