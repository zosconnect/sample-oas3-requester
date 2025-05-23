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

import java.util.ArrayList;
import java.util.List;

/**
 * A model class to represent an error where a requested IBM Redbook cannot be found
 */
public class RedbookNotFound {

    private String message;

    private List<Redbook> authorsWorks = new ArrayList<>();


    public RedbookNotFound(String message) {
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<Redbook> getAuthorsWorks() {
        return authorsWorks;
    }

    public void setAuthorsWorks(List<Redbook> authorsWorks) {
        this.authorsWorks = authorsWorks;
    }

    @Override
    public String toString() {
        return "Error 404 - " + message;
    }
}