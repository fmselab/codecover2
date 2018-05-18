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

package org.codecover.componenttest.model.testsessioncontainer.staticinformation;

import org.codecover.model.*;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDP0006.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDP0006 extends junit.framework.TestCase {
    /**
     * Loads two incompatible {@link TestSessionContainer}s and performs the
     * compatability check. If these checks return true, the testcase fails.
     */
    public void testCDP0006() {
        String singleContainerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        String multiContainerLocation = "../../qa/testdata/containers/multiplefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        try {
            TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                    builder, singleContainerLocation);
            TestSessionContainer tsc2 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                    builder, multiContainerLocation);

            assertFalse(tsc1.isCompatible(tsc2));
            assertFalse(tsc2.isCompatible(tsc1));

        } catch (FileLoadException e) {
            // Fail, if an exception was thrown
            fail();
        }
    }
}
