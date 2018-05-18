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

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: RemoveSessionsCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RemoveSessionsCommand extends Command {
    String containerId;

    TestSessionSet testSessions = null;

    /**
     * Sets the containerId.
     * 
     * @param containerId
     *                the containerId to set
     */
    public void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    /**
     * Adds a configured {@link TestSessionSet} to this command.
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
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        for (TestSession testSession : this.testSessions
                .getSessionMatches(testSessionContainer)) {
            testSession.delete();
        }
    }
}
