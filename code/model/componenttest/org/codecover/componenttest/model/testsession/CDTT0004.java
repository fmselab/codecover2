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
 * @version 1.0 ($Id: CDTT0004.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTT0004 extends junit.framework.TestCase {

    /**
     * CDTT0004: Copy test case
     */
    public void testCDTT0004() throws Exception {
        java.text.DateFormat format = new java.text.SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        //1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);
        
        //2.
        TestSessionContainer tsc2 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);
        
        //3.
        TestSession ts1 = tsc1.createTestSession("ts1", "", new Date());
        TestSession ts2 = tsc2.createTestSession("ts2", "", new Date());
        
        //4.
        Date date = format.parse("01.01.1970 00:00:01");
        TestCase tc1 = ts1.createTestCase("tc1", "test case 1", date, Collections.<CoverableItem, Long>emptyMap(), Collections.<CoverableItem, BooleanAssignmentMap>emptyMap());
        TestCase tc2 = ts2.createTestCase("tc2", "test case 2", date, Collections.<CoverableItem, Long>emptyMap(), Collections.<CoverableItem, BooleanAssignmentMap>emptyMap());
        
        //5.
        ts1.copyTestCaseIntoTestSession(tc2);
        
        //6.
        assertTrue(ts1.containsTestCaseWithName("tc2"));
        assertEquals("test case 2", ts1.getTestCaseWithName("tc2").getComment());
        assertEquals("01.01.1970 00:00:01", format.format(ts1.getTestCaseWithName("tc2").getDate()));
        
        //7.
        assertTrue(tc2.getTestSession() == ts2);
        assertTrue(ts1.getTestCaseWithName("tc2").getTestSession() == ts1);
    }
}
