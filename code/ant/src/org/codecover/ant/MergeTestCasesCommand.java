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
import org.codecover.model.TestCase;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.MergeException;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: MergeTestCasesCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MergeTestCasesCommand extends Command {
    String containerId;

    String name;

    String comment = "";

    boolean removeOldTestCases;

    TestCaseSet testCases = null;

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
     * Sets the removeOldTestCases.
     * 
     * @param removeOldTestCases
     *                the removeOldTestCases to set
     */
    public void setRemoveOldTestCases(boolean removeOldTestCases) {
        this.removeOldTestCases = removeOldTestCases;
    }

    /**
     * Adds a configured {@link TestCaseSet} to this command.
     * 
     * @param testCases
     *                the {@link TestCaseSet} to add.
     */
    public void addConfiguredTestCases(TestCaseSet testCases) {
        if (this.testCases != null) {
            throw new BuildException("There are multiple <testCases> elements");
        }
        this.testCases = testCases;
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

        List<TestCase> testCaseList = new ArrayList<TestCase>(this.testCases
                .getMatches(testSessionContainer));

        String mergedTestCaseName = this.name;
        String mergedTestCaseComment = this.comment;

        try {
            if (!testCaseList.isEmpty()) {
                testSessionContainer.mergeTestCases(testCaseList,
                        mergedTestCaseName, mergedTestCaseComment);

                // If the remove-option was set, delete the merged test cases
                if (this.removeOldTestCases) {
                    for (TestCase testCase : testCaseList) {
                        testCase.delete();
                    }
                }
            }
        } catch (MergeException e) {
            context.getLogger().fatal("Merging failed", e);
        }
    }
}
