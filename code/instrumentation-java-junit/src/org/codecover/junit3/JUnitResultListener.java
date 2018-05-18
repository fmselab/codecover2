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

package org.codecover.junit3;

import org.codecover.instrumentation.java.measurement.Protocol;

import junit.framework.TestListener;
import junit.runner.BaseTestRunner;
import junit.runner.TestCaseClassLoader;

/**
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: JUnitResultListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see JUnitResultListenerTestCase
 * @see JUnitResultListenerMethod
 * @see BaseTestRunner#getLoader()
 * @see TestCaseClassLoader
 */
public interface JUnitResultListener extends TestListener {

    /**
     * Calls {@link Protocol#endTestCase(String)}
     * for the last test case, if it has not been ended yet.
     */
    public void endLastOpenTestCase();
}
