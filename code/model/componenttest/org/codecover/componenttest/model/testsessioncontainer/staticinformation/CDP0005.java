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
 * @version 1.0 ($Id: CDP0005.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDP0005 extends junit.framework.TestCase {
    /**
     * Loads the same {@link TestSessionContainer} file twice into two different
     * instances, and checks if each is compatible to the other.
     */
    public void testCDP0005() {
        String containerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        try {
            TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                    builder, containerLocation);
            TestSessionContainer tsc2 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger,
                    builder, containerLocation);

            assertTrue(tsc1.isCompatible(tsc2));
            assertTrue(tsc2.isCompatible(tsc1));

        } catch (FileLoadException e) {
            // Fail, if an exception was thrown
            assertNotNull(null);
        }
    }
}
