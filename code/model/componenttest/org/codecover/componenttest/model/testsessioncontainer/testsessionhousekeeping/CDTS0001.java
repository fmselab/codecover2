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

import org.codecover.model.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDTS0001.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTS0001 extends junit.framework.TestCase {
    /**
     * CDTS0001: No Test Sessions
     * @throws Exception 
     */
    public void testCDTS0001() throws Exception {
        //1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                builder, singleContainerLocation);
        
        //2.
        assertTrue(tsc.getTestSessions().isEmpty());
        
        //3.
        assertTrue(tsc.getTestSessionNames().isEmpty());
        
        //4.
        assertFalse(tsc.containsTestSessionWithName("foobar"));
        assertFalse(tsc.containsTestSessionWithName(""));
        
        //5.
        assertNull(tsc.getTestSessionWithName("foobar"));
        assertNull(tsc.getTestSessionWithName(""));
    }
}
