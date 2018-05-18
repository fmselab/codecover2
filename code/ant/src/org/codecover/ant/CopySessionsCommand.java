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

package org.codecover.ant;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CopySessionsCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CopySessionsCommand extends Command {
    String sourceContainerId;

    String destinationContainerId;

    TestSessionSet testSessions = null;

    /**
     * Sets the sourceContainerId.
     * 
     * @param sourceContainerId
     *                the sourceContainerId to set
     */
    public void setSourceContainerId(String sourceContainerId) {
        this.sourceContainerId = sourceContainerId;
    }

    /**
     * Sets the destinationContainerId.
     * 
     * @param destinationContainerId
     *                the destinationContainerId to set
     */
    public void setDestinationContainerId(String destinationContainerId) {
        this.destinationContainerId = destinationContainerId;
    }

    /**
     * Adds the configures {@link TestSessionSet} to this command.
     * 
     * @param testSessions
     *                the {@link TestSessionSet} to add.
     */
    public void addConfiguredTestSessions(TestSessionSet testSessions) {
        if (this.testSessions != null) {
            throw new BuildException(
                    "There are multiple <testSessions> elements");
        }
        this.testSessions = testSessions;
    }

    @Override
    public void run(Context context) {
        if (this.sourceContainerId == null) {
            throw new BuildException(
                    "The attribute 'sourceContainerId' is missing.");
        }

        if (this.destinationContainerId == null) {
            throw new BuildException(
                    "The attribute 'destinationContainerId' is missing.");
        }

        TestSessionContainer sourceTestSessionContainer = context
                .getTestSessionContainer(this.sourceContainerId);
        TestSessionContainer destinationTestSessionContainer = context
                .getTestSessionContainer(this.destinationContainerId);

        if (!sourceTestSessionContainer
                .isCompatible(destinationTestSessionContainer)) {
            context.getLogger().fatal(
                    "sourceTestSessionContainer is incompatible"
                            + "  with destinationTestSessionContainer");
        }

        List<TestSession> sessionList = new ArrayList<TestSession>(
                this.testSessions.getSessionMatches(sourceTestSessionContainer));

        // traverse the found sessions of the sourceTestSessionContainer and
        // copy them into the destinationTestSessionContainer.
        for (TestSession sourceTestSession : sessionList) {
            String testSessionName = sourceTestSession.getName();
            String testSessionComment = sourceTestSession.getComment();
            Date testSessionDate = sourceTestSession.getDate();

            // Create the destination test session with the data from the old
            // test session
            TestSession destinationTestSession = destinationTestSessionContainer
                    .createTestSession(testSessionName, testSessionComment,
                            testSessionDate);

            // If the name of the test session already existed, the user should
            // know, that the session is called something like
            // %test session name (1)%
            if (!testSessionName.equals(destinationTestSession.getName())) {
                context
                        .getLogger()
                        .warning(
                                "A test session with the name \""
                                        + testSessionName
                                        + "\" already exisited! The copied test session can be found under the name: \""
                                        + destinationTestSession.getName()
                                        + "\"");
            }

            // Copy each test case from the source test session into the
            // destination test session
            for (TestCase sourceTestCase : sourceTestSession.getTestCases()) {
                destinationTestSession
                        .copyTestCaseIntoTestSession(sourceTestCase);
            }
        }
    }
}
