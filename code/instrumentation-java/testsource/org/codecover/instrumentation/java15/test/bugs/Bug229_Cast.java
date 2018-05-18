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
 * @version 1.0 ($Id: Bug229_Cast.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * For https://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=229.
 * 
 * InstrumenterException when instrumenting conditional expressions with casts.
 */
public class Bug229_Cast {
    public static void foo(Object o) {
        if ((Boolean) o) ;
    }
}
