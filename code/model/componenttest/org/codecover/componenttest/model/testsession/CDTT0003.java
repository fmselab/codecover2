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

import java.util.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDTT0003.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTT0003 extends junit.framework.TestCase {

    /**
     * CDTT0003: Creating test cases
     */
    public void testCDTT0003() throws Exception {
        //1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);
        
        //2.
        TestSession ts1 = tsc.createTestSession("ts1", "", new Date());
        
        //3.
        Date now = new Date();
        TestCase tc1 = ts1.createTestCase("tc1", "test case 1", now, Collections.<CoverableItem, Long>emptyMap(), Collections.<CoverableItem, BooleanAssignmentMap>emptyMap());
        
        //4.
        assertEquals(ts1.getTestCases().size(), 1);
        assertEquals(ts1.getTestCaseNames().size(), 1);
        assertEquals(ts1.getTestCaseNames().get(0), "tc1");
        
        assertEquals(tc1.getName(), "tc1");
        assertEquals(ts1.getTestCases().get(0).getName(), "tc1");
        assertEquals(tc1.getComment(), "test case 1");
        assertEquals(ts1.getTestCases().get(0).getComment(), "test case 1");
        assertEquals(tc1.getDate(), now);
        assertEquals(ts1.getTestCases().get(0).getDate(), now);
        assertTrue(tc1.getTestSession() == ts1);
        assertTrue(ts1.getTestCases().get(0).getTestSession() == ts1);

        //5.
        assertTrue(ts1.getTestCaseWithName("tc1") == tc1);
        assertTrue(ts1.containsTestCaseWithName("tc1"));
    }
}

