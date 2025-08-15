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
package com.ibm.zosconnect.redbook.model;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * A model class that represent the metadata of an IBM Redbook
 */
public class Redbook {

    private String title;

    private List<Author> authors = new ArrayList<>();

    private String formNumber;

    public enum Status { DRAFT, PUBLISHED };
    private Status status = Status.DRAFT;

    private Date publicationDate = new Date();

    public enum DocumentType { PDF, HARDCOPY };
    private DocumentType documentType = DocumentType.PDF;

    private double sizeMB;

    private URL url;

    private OwningDepartment owningDepartment;

    public Redbook(){}

    public Redbook(String title, List<Author> authors, String formNumber, String type, OwningDepartment dept) {
        this.title = title;
        this.authors = authors;
        this.formNumber = formNumber;
        this.documentType = DocumentType.valueOf(type);
        this.owningDepartment = dept;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public List<Author> getAuthors() {
        return authors;
    }

    public void setAuthors(List<Author> authors) {
        this.authors = authors;
    }

    public String getFormNumber() {
        return formNumber;
    }

    public void setFormNumber(String formNumber) {
        this.formNumber = formNumber;
    }

    public Date getPublicationDate() {
        return publicationDate;
    }

    public void setPublicationDate(Date publicationDate) {
        this.publicationDate = publicationDate;
    }

    public double getSizeMB() {
        return sizeMB;
    }

    public void setSizeMB(double sizeMB) {
        this.sizeMB = sizeMB;
    }

    public URL getUrl() {
        return url;
    }

    public void setUrl(String url) {
        try {
            this.url = new URL(url);
        } catch (MalformedURLException e) {
            /*
             * Test data URLs have no errors
             */
            e.printStackTrace();
        }
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    public DocumentType getDocumentType() {
        return documentType;
    }

    public void setDocumentType(DocumentType documentType) {
        this.documentType = documentType;
    }

    public OwningDepartment getOwningDepartment() {
        return owningDepartment;
    }

    public void setOwningDepartment(OwningDepartment owningDepartment) {
        this.owningDepartment = owningDepartment;
    }

    @Override
    public String toString() {
        return "Redbook " + title+ " " + authors + " - " + formNumber;
    }
}
