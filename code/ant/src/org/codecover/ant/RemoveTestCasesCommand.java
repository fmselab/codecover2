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
import org.codecover.model.TestSessionContainer;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: RemoveTestCasesCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class RemoveTestCasesCommand extends Command {
    String containerId;

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

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        for (TestCase testCase : this.testCases
                .getMatches(testSessionContainer)) {
            testCase.delete();
        }
    }
}
