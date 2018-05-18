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

package org.codecover.componenttest.model.testsessioncontainer.testsessionhousekeeping;

import java.util.*;

import org.codecover.model.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDTS0003.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTS0003 extends junit.framework.TestCase {
    /**
     * CDTS0003: Merging empty Test Session
     * 
     * @throws Exception
     */
    public void testCDTS0003() throws Exception {
        // 1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                singleContainerLocation);

        // 2.
        Date now1 = new Date();
        TestSession ts1 = tsc.createTestSession("ts1", "test session1", now1);

        // 3.
        Date now2 = new Date();
        TestSession ts2 = tsc.createTestSession("ts2", "test session2", now2);

        // 4.
        final List<TestSession> sessions = new ArrayList<TestSession>();
        sessions.add(ts1);
        sessions.add(ts2);
        Date before = new Date();
        TestSession ts3 = tsc.mergeTestSessions(sessions, "ts3",
                "merged test sessions 3");
        Date after = new Date();
        assertEquals(ts3.getName(), "ts3");
        assertEquals(ts3.getComment(), "merged test sessions 3");
        assertFalse(before.after(ts3.getDate()));
        assertFalse(after.before(ts3.getDate()));
        assertTrue(ts3.getTestCases().isEmpty());

        // 5.
        assertTrue(tsc.containsTestSessionWithName("ts1"));
        assertTrue(tsc.containsTestSessionWithName("ts2"));
        assertTrue(tsc.containsTestSessionWithName("ts3"));
    }
}
