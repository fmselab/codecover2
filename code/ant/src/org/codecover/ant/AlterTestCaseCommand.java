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
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.NameAlreadyUsedException;

/**
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: AlterTestCaseCommand.java 50 2009-06-01 09:59:34Z ahija $)
 *
 */
public class AlterTestCaseCommand extends Command {
    String containerId;

    String session;

    String testCaseName;

    String name;

    String comment;

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
     * Sets the session.
     *
     * @param session
     *                the session to set
     */
    public void setSession(String session) {
        this.session = session;
    }

    /**
     * Sets the testCase.
     *
     * @param testCase
     *                the testCase to set
     */
    public void setTestCase(String testCase) {
        this.testCaseName = testCase;
    }

    /**
     * Sets the name.
     *
     * @param name
     *                the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Sets the comment.
     *
     * @param comment
     *                the comment to set
     */
    public void setComment(String comment) {
        this.comment = comment;
    }

    @Override
    public void run(Context context) {
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        if (this.session == null) {
            throw new BuildException("The attribute 'session' is missing.");
        }

        if (this.testCaseName == null) {
            throw new BuildException("The attribute 'testCase' is missing.");
        }

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        TestSession testSession = testSessionContainer
                .getTestSessionWithName(this.session);

        if (testSession == null) {
            context.getLogger().fatal(
                    "Session '" + this.session + "' not found.");
        }

        TestCase testCase = testSession.getTestCaseWithName(this.testCaseName);

        if (this.name != null) {
            String newTestCaseName = this.name;
            try {
                testCase.setName(newTestCaseName);
                context.getLogger().info(
                        "Changed name of test case from \"" + testCase
                                + "\" to \"" + newTestCaseName + "\".");
            } catch (NameAlreadyUsedException e) {
                context.getLogger().warning("Name could not be set", e);
            }
        }

        if (this.comment != null) {
            String oldTestCaseComment = testCase.getComment();
            String newTestCaseComment = this.comment;
            testCase.setComment(newTestCaseComment);
            context.getLogger().info(
                    "Changed comment of test case from \"" + oldTestCaseComment
                            + "\" to \"" + newTestCaseComment + "\".");
        }
    }
}
