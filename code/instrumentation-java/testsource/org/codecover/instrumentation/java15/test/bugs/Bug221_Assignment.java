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

package org.codecover.instrumentation.java15.test.bugs;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: Bug221_Assignment.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * For https://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=221. "Assignments"
 */
public class Bug221_Assignment {
    public Object bar() {
        return null;
    }

    public Object foo(Object b) throws Exception {
        Object a;
        if (b == null || ((a = bar()) == null)) {
            throw new Exception();
        }
        return a.toString();
    }

    public int foo(int i) {
        int j;
        if ((j = i) == 1) {
            return j;
        }
        return 0;
    }

    public void foo() {
        boolean b = true;
        boolean a = false;
        if (a = b) {
            
        }
        if (a |= b) {
            
        }
        if (a &= b) {
            
        }
        if (a ^= b) {
            
        }
        int i = 0;
        if  ((i -= 8) < 0) {
            
        }
    }
}
