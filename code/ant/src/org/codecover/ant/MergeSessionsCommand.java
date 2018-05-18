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
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.MergeException;
import org.codecover.model.utils.NameReoccurenceHelper;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: MergeSessionsCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeSessionsCommand extends Command {
    String containerId;

    String name;

    String comment = "";

    boolean removeOldSessions;

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

    /**
     * Sets the removeOldSessions.
     * 
     * @param removeOldSessions
     *                the removeOldSessions to set
     */
    public void setRemoveOldSessions(boolean removeOldSessions) {
        this.removeOldSessions = removeOldSessions;
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

        if (this.name == null) {
            throw new BuildException("The attribute 'name' is missing.");
        }

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        List<TestSession> testSessionList = new ArrayList<TestSession>(
                this.testSessions.getSessionMatches(testSessionContainer));

        String mergedTestSessionName = this.name;

        String escapedName = NameReoccurenceHelper.escapeName(
                testSessionContainer.getTestSessionNames(),
                mergedTestSessionName);

        // The name of the merged test session is already present in the test
        // session container, so an (count) is appended
        if (!mergedTestSessionName.equals(escapedName)) {
            context
                    .getLogger()
                    .warning(
                            "A test session with the name \""
                                    + mergedTestSessionName
                                    + "\" already exists! The merged test session is saved under the name \""
                                    + escapedName + "\"");
        }

        String mergedTestSessionComment = this.comment;

        try {
            if (!testSessionList.isEmpty()) {
                testSessionContainer.mergeTestSessions(testSessionList,
                        mergedTestSessionName, mergedTestSessionComment);

                // If the remove-option was set, delete the merged test sessions
                if (this.removeOldSessions) {
                    for (TestSession testSession : testSessionList) {
                        testSession.delete();
                    }
                }

            } else {
                context
                        .getLogger()
                        .warning(
                                "No test sessions merged, since no valid test session names were given!");
            }
        } catch (MergeException e) {
            context.getLogger().fatal("An error occured during merging", e);
        }
    }
}
