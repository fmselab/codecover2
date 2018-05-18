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
 * @version 1.0 ($Id: ExampleProgramer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExampleProgramer extends ExamplePerson {
    
    public ExampleProgramer() {
        super.access();
        super.setFamilyName("");
        super.setGivenName("");
    }

    public void setFamilyName(String familyName) {
        super.access();
        if (familyName.equals("Müller")) {
            familyName = "Mueller";
        }
        super.setFamilyName(familyName);
    }
    
    public void setSalary(double salary) {
        super.access();
        super.setSalary(salary * 1.1);
    }
}
