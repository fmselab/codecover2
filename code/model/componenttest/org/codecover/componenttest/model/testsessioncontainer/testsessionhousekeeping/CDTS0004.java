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
import org.codecover.model.exceptions.ModelException;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDTS0004.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTS0004 extends junit.framework.TestCase {
    /**
     * CDTS0004: Merging empty Test Sessions from incompatible TSCs
     * 
     * @throws Exception
     */
    public void testCDTS0004() throws Exception {
        // 1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                singleContainerLocation);

        // 2.
        TestSession ts1 = tsc.createTestSession("ts1", "test session1",
                new Date());

        // 3.
        String statementBranchContainerLocation = "../../qa/testdata/containers/singlefile/statement-branch.xml";
        TestSessionContainer tsc2 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                statementBranchContainerLocation);

        // 4.
        TestSession ts2 = tsc2.createTestSession("ts2", "test session2",
                new Date());

        // 5.
        final List<TestSession> sessions = new ArrayList<TestSession>();
        sessions.add(ts1);
        sessions.add(ts2);
        try {
            tsc.mergeTestSessions(sessions, "ts3", "merged test sessions 3");
            fail();
        } catch (ModelException e) {
        }
    }
}
