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
 * @version 1.0 ($Id: CDLS0016.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0016 extends junit.framework.TestCase {
    /**
     * CDLS0016: Load - invalid test cases
     */
    public void testCDLS0016() throws Exception {
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        
        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, "../../qa/testdata/containers/session/test-case-invalid-item-ref.xml");
            fail();
        } catch (FileLoadParseException e) {
        }
        
        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, "../../qa/testdata/containers/session/test-case-invalid-rootterm-ref.xml");
            fail();
        } catch (FileLoadParseException e) {
        }
        
        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, "../../qa/testdata/containers/session/test-case-invalid-rootterm-arity.xml");
            fail();
        } catch (FileLoadParseException e) {
        }
        
        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, "../../qa/testdata/containers/session/test-case-inconsistent-arity.xml");
            fail();
        } catch (FileLoadParseException e) {
        }
    }
}
