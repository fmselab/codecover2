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
 * @version 1.0 ($Id: Bug232_ParseError.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * http://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=232
 * Parse Error in Java Instrumenter.
 */
public class Bug232_ParseError {
    public static void main(String[] args) {

        Class c0 = (int.class);
              c0 = (int[].class);
              c0 = (Object.class);
              c0 = (Object[].class);
              c0 = (java.lang.Class)c0;
              c0 = void.class;

        Class c1 = (int.class),
              c2 = (int[].class),
              c3 = (Object.class),
              c4 = (Object[].class),
              c5 = void.class;
    }   
}