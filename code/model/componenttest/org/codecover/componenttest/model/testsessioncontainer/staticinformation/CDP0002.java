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

import java.util.*;

import org.codecover.model.*;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDP0002.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDP0002 extends junit.framework.TestCase {
    /**
     * Loads all the {@link TestSessionContainer}s in the singlefile location
     * and checks, if they all contain a single sourceFile and that sourcefile
     * is of a certain length and name.
     */
    public void testCDP0002() {
        String containerLocation = "../../qa/testdata/containers/singlefile/";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);

        try {
            String[] containerNames = new String[] { "no-criteria.xml",
                    "statement.xml", "branch.xml", "statement-branch.xml",
                    "loop.xml", "condition.xml", "all-criteria.xml" };
            List<TestSessionContainer> containerList = new Vector<TestSessionContainer>();

            // Load all the testSessionContainers
            for (String containerName : containerNames) {
                containerList.add(TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder,
                        containerLocation + containerName));
            }

            List<SourceFile> sourceFileList;
            final String fileName = "TestClass1.java";
            final int contentLength = 2417;
            final int listLength = 1;

            for (TestSessionContainer testSessionContainer : containerList) {
                sourceFileList = testSessionContainer.getFiles();
                assertNotNull(sourceFileList);
                assertEquals(sourceFileList.size(), listLength);
                assertEquals(sourceFileList.get(0).getFileName(), fileName);
                assertEquals(sourceFileList.get(0).getContent().length(),
                        contentLength);
            }

        } catch (FileLoadException e) {
            // Fail, if an exception was thrown
            assertNotNull(null);
        }

    }
}
