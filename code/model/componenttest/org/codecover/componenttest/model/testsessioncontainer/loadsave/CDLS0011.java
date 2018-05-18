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
 * @version 1.0 ($Id: CDLS0011.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0011 extends junit.framework.TestCase {
    /**
     * CDLS0011: Load - single empty test session
     */
    public void testCDLS0011() throws Exception {
        java.text.DateFormat format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss zzz");
        format.setTimeZone(new SimpleTimeZone(0, "GMT"));

        // 1.
        String sessionEmpty = "../../qa/testdata/containers/session/empty-session.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                sessionEmpty);
        
        //2.
        assertTrue(tsc.containsTestSessionWithName("ts1"));
        TestSession ts1 = tsc.getTestSessionWithName("ts1");
        assertEquals("test session 1", ts1.getComment());
        assertEquals("2000-01-01 11:00:00 GMT", format.format(ts1.getDate()));
        
        //3.
        assertEquals(1, tsc.getTestSessions().size());
        assertTrue(tsc.getTestSessions().get(0) == tsc.getTestSessionWithName("ts1"));
        
        //4.
        assertTrue(ts1.getTestCases().isEmpty());
    }
}
