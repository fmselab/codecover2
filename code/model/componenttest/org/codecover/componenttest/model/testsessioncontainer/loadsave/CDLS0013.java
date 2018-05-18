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

package org.codecover.componenttest.model.testsessioncontainer.loadsave;

import java.util.*;
import java.io.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDLS0013.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0013 extends junit.framework.TestCase {
    /**
     * CDLS0013: Load - test session with a single empty test case
     */
    public void testCDLS0013() throws Exception {
        java.text.DateFormat format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss zzz");
        format.setTimeZone(new SimpleTimeZone(0, "GMT"));

        // 1.
        String sessionEmptyTestCase = "../../qa/testdata/containers/session/empty-test-case.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                sessionEmptyTestCase);
        
        // 2.
        assertTrue(tsc.containsTestSessionWithName("ts2"));
        TestSession ts2 = tsc.getTestSessionWithName("ts2");
        
        // 3.
        assertTrue(ts2.containsTestCaseWithName("tc1"));
        TestCase tc1 = ts2.getTestCaseWithName("tc1");
        assertEquals("test case 1", tc1.getComment());
        assertEquals("2000-01-01 12:00:01 GMT", format.format(tc1.getDate()));
        
        // 4.
        assertEquals(1, ts2.getTestCases().size());
    }
}
