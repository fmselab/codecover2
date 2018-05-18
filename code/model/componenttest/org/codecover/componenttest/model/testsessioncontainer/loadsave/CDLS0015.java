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
 * @version 1.0 ($Id: CDLS0015.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0015 extends junit.framework.TestCase {
    /**
     * CDLS0015: Load - valid test case
     */
    public void testCDLS0015() throws Exception {
        java.text.DateFormat format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss zzz");
        format.setTimeZone(new SimpleTimeZone(0, "GMT"));

        // 1.
        String sessionTestCase = "../../qa/testdata/containers/session/test-case.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                sessionTestCase);
        
        // 2.
        assertTrue(tsc.containsTestSessionWithName("ts2"));
        TestSession ts2 = tsc.getTestSessionWithName("ts2");
        
        // 3.
        assertTrue(ts2.containsTestCaseWithName("tc2"));
        TestCase tc2 = ts2.getTestCaseWithName("tc2");
        
        // 4.
        assertEquals(10, tc2.getCoverageCount(builder.createCoverableItem("org.codecover.tests.TestClass2.java", "S8")));
        
        // 5.
        BooleanAssignmentMap map = tc2.getAssignmentsMap().get(builder.createCoverableItem("org.codecover.tests.TestClass2.java", "C2"));
        assertEquals(1, map.get(new BooleanAssignment(BooleanResult.TRUE, BooleanResult.FALSE)));
        assertEquals(1, map.get(new BooleanAssignment(BooleanResult.FALSE, BooleanResult.FALSE)));
    }
}
