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

import java.io.Serializable;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: Bug253_TwoExtends.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * http://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=253
 * Parse Error in Java Instrumenter
 */
public class Bug253_TwoExtends {
    static interface Bug253Interface extends Serializable, Comparable<String> {
        public void makeSomething();
    }
}