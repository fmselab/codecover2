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

package org.codecover.junit4;

import junit.framework.Assert;

import org.codecover.classes.ExampleProgramer;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: ExampleProgramerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExampleProgramerTest {

    ExampleProgramer exampleProgrammer;

    /**
     *
     * @throws java.lang.Exception
     */
    @Before
    public void before() throws Exception {
        this.exampleProgrammer = new ExampleProgramer();
    }

    /**
     *
     * @throws java.lang.Exception
     */
    @After
    public void after() throws Exception {
        this.exampleProgrammer = null;
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getFamilyName()}.
     */
    @Test
    public final void getFamilyName() {
        Assert.assertNotNull(this.exampleProgrammer.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setFamilyName(java.lang.String)}.
     */
    @Test
    public final void setFamilyName() {
        String familyName = "Junkers";
        this.exampleProgrammer.setFamilyName(familyName);
        Assert.assertSame(familyName, this.exampleProgrammer.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setFamilyName(java.lang.String)}.
     */
    @Test
    public final void setFamilyName2() {
        String familyName = "Müller";
        this.exampleProgrammer.setFamilyName(familyName);
        Assert.assertEquals("Mueller", this.exampleProgrammer.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getGivenName()}.
     */
    @Test
    public final void getGivenName() {
        Assert.assertNotNull(this.exampleProgrammer.getGivenName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setGivenName(java.lang.String)}.
     */
    @Test
    public final void setGivenName() {
        String given = "Enrico";
        this.exampleProgrammer.setGivenName(given);
        Assert.assertSame(given, this.exampleProgrammer.getGivenName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getSalary()}.
     */
    @Test
    public final void getSalary() {
        Assert.assertEquals(0.0, this.exampleProgrammer.getSalary(), 0.0);
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setSalary(double)}.
     */
    @Test
    public final void setSalary() {
        double salary = 40000;
        this.exampleProgrammer.setSalary(salary);
        salary *= 1.1;
        Assert.assertEquals(salary, this.exampleProgrammer.getSalary(), 3999.0);
    }

    @Test
    public final void exception() {
        this.exampleProgrammer.getFamilyName().toString();
    }
}
