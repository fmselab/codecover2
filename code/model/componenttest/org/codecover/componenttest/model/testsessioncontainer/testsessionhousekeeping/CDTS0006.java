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

import java.util.ArrayList;

import org.codecover.model.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDTS0006.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDTS0006 extends junit.framework.TestCase {
    /**
     * CDTS0006: Merging an empty list of Test Sessions
     * 
     * @throws FileLoadException
     * 
     */
    public void testCDTS0006() throws FileLoadException {
        // 1.
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        // simply use singlefile/statement.xml for this test
        TestSessionContainer tsc = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                singleContainerLocation);
        try {
            tsc.mergeTestSessions(new ArrayList<TestSession>(), "ts3",
                    "merged test sessions 3");
            // Fail, if no exception was thrown
            fail();
        } catch (MergeException e) {
            fail();
        } catch (IllegalArgumentException e) {

        }
    }
}
