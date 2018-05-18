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

import java.util.Date;

import org.codecover.model.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDTS0002.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTS0002 extends junit.framework.TestCase {
    /**
     * CDTS0002: Creating Test Sessions
     * @throws Exception 
     */
    public void testCDTS0002() throws Exception {
        //1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);
        
        //2.
        Date now = new Date();
        TestSession ts1 = tsc.createTestSession("ts1", "test session1", now);
        
        //3.
        assertEquals(tsc.getTestSessionNames().size(), 1);
        assertEquals(tsc.getTestSessionNames().get(0), "ts1");
        
        //4.
        assertEquals(tsc.getTestSessions().size(), 1);
        assertEquals(tsc.getTestSessions().get(0), ts1);
        
        //5.
        assertEquals(tsc.getTestSessionWithName("ts1"), ts1);
        
        //6.
        assertEquals(ts1.getName(), "ts1");
        assertEquals(tsc.getTestSessions().get(0).getName(), "ts1");
        assertEquals(ts1.getComment(), "test session1");
        assertEquals(tsc.getTestSessions().get(0).getComment(), "test session1");
        assertEquals(ts1.getDate(), now);
        assertEquals(tsc.getTestSessions().get(0).getDate(), now);
        assertTrue(ts1.getTestSessionContainer() == tsc);
        assertTrue(tsc.getTestSessions().get(0).getTestSessionContainer() == tsc);
        
        //7.
        assertTrue(tsc.containsTestSessionWithName("ts1"));
        assertFalse(tsc.containsTestSessionWithName("ts2"));
    }
}
