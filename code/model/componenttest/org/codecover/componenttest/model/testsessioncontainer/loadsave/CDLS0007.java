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
import java.util.Date;

import org.codecover.model.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDLS0007.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0007 extends junit.framework.TestCase {
    /**
     * Loads the statement.xml {@link TestSessionContainer}, creates a new
     * {@link TestSession} in it, and saves the {@link TestSessionContainer} to
     * a new location. The saved {@link TestSessionContainer} is loaded again,
     * and it is checked, if the specified {@link TestSession} is part of the
     * {@link TestSessionContainer}.
     */
    public void testCDLS0007() throws Exception {
        String containerLocation = "../../qa/testdata/containers/singlefile/statement.xml";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer testSessionContainer = TestSessionContainer
                .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation);

        String testSessionName = "ts1";
        String testSessionComment = "test session 1";
        Date testSessionDate = new Date();

        testSessionContainer.createTestSession(testSessionName,
                testSessionComment, testSessionDate);

        String newLocation = "../../qa/testdata/containers/singlefile/statement-with-session.xml";
        testSessionContainer.save(newLocation);

        // Reload the container and get the session with the name specified
        // above.
        TestSessionContainer reloadedContainer = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), 
                logger, builder, newLocation);
        TestSession testSession = reloadedContainer
                .getTestSessionWithName(testSessionName);

        // Assert, that the session has the same data.
        assertNotNull(testSession);
        assertEquals(testSession.getName(), testSessionName);
        assertEquals(testSession.getComment(), testSessionComment);
        assertEquals(testSession.getDate(), testSessionDate);

        // finally we delete the sstatement-with-session.xml cause this would be
        // under version control
        assertTrue(new File(newLocation).delete());
    }
}
