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

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.utils.ReportUtils;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: ReportCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class ReportCommand extends Command {
    String containerId;

    File template;

    File destination;

    boolean override = true;

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
     * Sets the template.
     * 
     * @param template
     *                the template to set
     */
    public void setTemplate(File template) {
        this.template = template;
    }

    /**
     * Sets the destination.
     * 
     * @param destination
     *                the destination to set
     */
    public void setDestination(File destination) {
        this.destination = destination;
    }

    /**
     * Sets the override.
     * 
     * @param override
     *                the override to set
     */
    public void setOverride(boolean override) {
        this.override = override;
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

        if (this.template == null) {
            throw new BuildException("The attribute 'template' is missing.");
        }

        if (this.destination == null) {
            throw new BuildException("The attribute 'destination' is missing.");
        }

        if (this.testCases == null) {
            throw new BuildException("The <testCases> element is missing.");
        }

        // TODO: Use override

        final ProgressHandler progressHandler = ProgressHandler.NULL; // TODO?

        context.getLogger().info("Generating Report:");
        context.getLogger()
                .info("template: " + this.template.getAbsolutePath());
        context.getLogger().info(
                "destination: " + this.destination.getAbsolutePath());

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        List<TestCase> testCases = new ArrayList<TestCase>(this.testCases
                .getMatches(testSessionContainer));

        ReportUtils.report(context.getLogger(), context.getPluginManager(),
                progressHandler, false, this.destination, this.template,
                testCases);
    }
}
