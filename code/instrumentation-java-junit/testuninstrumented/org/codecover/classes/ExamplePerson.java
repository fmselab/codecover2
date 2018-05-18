/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.classes;

/**
 * Usage: To be instrumented for TestRunner tests by build.xml, target <code>instrument-test</code>
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: ExamplePerson.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExamplePerson {

    private String familyName;

    private String givenName;

    private double salary;

    private int accessCount = 0;

    public String getFamilyName() {
        access();
        return this.familyName;
    }

    public void setFamilyName(String familyName) {
        access();
        this.familyName = familyName;
    }

    public String getGivenName() {
        access();
        return this.givenName;
    }

    public void setGivenName(String givenName) {
        access();
        this.givenName = givenName;
    }

    public double getSalary() {
        access();
        return this.salary;
    }

    public void setSalary(double salary) {
        access();
        this.salary = salary;
    }
    
    protected void access() {
        this.accessCount++;
    }
}
