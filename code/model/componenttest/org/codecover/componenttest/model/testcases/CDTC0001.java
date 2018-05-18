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

package org.codecover.componenttest.model.testcases;

import java.util.*;
import java.io.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDTC0001.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTC0001 extends junit.framework.TestCase {
    /**
     * CDTC0001: Deleting test cases
     */
    public void testCDTC0001() throws Exception {
        // 1.
        String sessionTestCase = "../../qa/testdata/containers/session/test-case.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                sessionTestCase);
        
        // 2.
        TestCase tc1 = tsc.getTestSessionWithName("ts2").getTestCaseWithName("tc1");
        
        // 3.
        tc1.delete();
        
        // 4.
        assertFalse(tsc.getTestSessionWithName("ts2").containsTestCaseWithName("tc1"));
        
        // 5.
        try {
            tc1.getTestSession();
            fail();
        } catch (IllegalStateException e) {
        }
    }
}
