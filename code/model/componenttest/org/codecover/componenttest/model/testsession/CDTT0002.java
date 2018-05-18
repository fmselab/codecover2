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

package org.codecover.componenttest.model.testsession;

import java.util.Date;

import junit.framework.TestCase;

import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.SimpleLogger;

/**
 * 
 * @author Markus Wittlinger
 * @author Robert Hanussek
 * @version 1.0 ($Id: CDTT0002.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTT0002 extends TestCase {
    
    /**
     * CDTT0002: Empty test sessions
     * <p>
     * Loads a <code>TestSessionContainer</code> from a file, creates an empty
     * test session in it and checks if the created test session is really empty
     * (that is, the test session doesn't contain any test cases).
     */
    public void testCDTT0002() throws FileLoadException {

        String singleContainerLocation
                = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);

        // 1.
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);

        // 2.
        TestSession ts = tsc.createTestSession("ts1",
                "test session1", new Date());

        // 3.
        assertTrue(ts.getTestCases().isEmpty());
        assertTrue(ts.getTestCaseNames().isEmpty());

        // 4.
        assertNull(ts.getTestCaseWithName("foobar"));
        assertNull(ts.getTestCaseWithName(""));
        assertFalse(ts.containsTestCaseWithName("foobar"));
        assertFalse(ts.containsTestCaseWithName(""));
    }
}
