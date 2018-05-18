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

import java.io.File;

import org.codecover.model.*;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDLS0003.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0003 extends junit.framework.TestCase {
    /**
     * Tries to load a file, that does not exists, using both of the provided
     * load methods.
     * <p>
     * If no {@link FileLoadException} is thrown during the loading process, the
     * testcase will fail.
     * 
     */
    public void testCDLS0003() {
        String containerLocation = "buildnoexists.xml";
        File containerFile = new File(containerLocation);
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation);
            // Fail, if no exception was thrown
            assertNotNull(null);
        } catch (FileLoadException e) {
        }

        try {
            TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerFile);
            // Fail, if no exception was thrown
            assertNotNull(null);
        } catch (FileLoadException e) {
        }
    }
}
