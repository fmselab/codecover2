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

import org.codecover.classes.ExamplePerson;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: ExamplePersonTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExamplePersonTest {

    ExamplePerson examplePerson;

    /**
     *
     * @throws java.lang.Exception
     */
    @Before
    public void before() throws Exception {
        this.examplePerson = new ExamplePerson();
    }

    /**
     *
     * @throws java.lang.Exception
     */
    @After
    public void after() throws Exception {
        this.examplePerson = null;
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getFamilyName()}.
     */
    @Test
    public final void getFamilyName() {
        Assert.assertNull(this.examplePerson.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setFamilyName(java.lang.String)}.
     */
    @Test
    public final void setFamilyName() {
        String familyName = "Junkers";
        this.examplePerson.setFamilyName(familyName);
        Assert.assertSame(familyName, this.examplePerson.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setFamilyName(java.lang.String)}.
     */
    @Test
    public final void setFamilyName2() {
        String familyName = "Müller";
        this.examplePerson.setFamilyName(familyName);
        Assert.assertEquals("Mueller", this.examplePerson.getFamilyName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getGivenName()}.
     */
    @Test
    public final void getGivenName() {
        Assert.assertNull(this.examplePerson.getGivenName());
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setGivenName(java.lang.String)}.
     */
    @Test
    public final void setGivenName() {
        String given = "Enrico";
        this.examplePerson.setGivenName(given);
        Assert.assertSame(given, this.examplePerson.getGivenName());
        Assert.fail("just fail");
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#getSalary()}.
     */
    @Test
    public final void getSalary() {
        Assert.assertEquals(0.0, this.examplePerson.getSalary(), 0.0);
    }

    /**
     * Test method for {@link org.codecover.junit.classes.ExamplePerson#setSalary(double)}.
     */
    @Test
    public final void setSalary() {
        double salary = 40000;
        this.examplePerson.setSalary(salary);
        salary *= 1.1;
        Assert.assertEquals(salary, this.examplePerson.getSalary(), 3999.0);
    }

    @Test
    public final void exception() {
        this.examplePerson.getFamilyName().toString();
    }
}
