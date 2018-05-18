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
package org.codecover.juniteclipse;

import org.codecover.junit.JUnitFailureContainer;
import org.eclipse.jdt.internal.junit.runner.IListensToTestExecutions;
import org.eclipse.jdt.internal.junit.runner.MessageIds;
import org.eclipse.jdt.internal.junit.runner.TestReferenceFailure;

/**
 * A {@link JUnitFailureContainer} for JUnit 4.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: JUnitEclipseFailureContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JUnitEclipseFailureContainer extends JUnitFailureContainer {
    private String content;
    
    /**
     * Sets the properties by analyzing the thrown Failure.
     *
     * @param failure The {@link TestReferenceFailure} of
     * {@link IListensToTestExecutions#notifyTestFailed(TestReferenceFailure)}. 
     */
    public JUnitEclipseFailureContainer(TestReferenceFailure failure) {
        super();

        // set the type
        String status = failure.getStatus();
        if (status == MessageIds.TEST_ERROR) {
            super.setFailureType(FAILURE_TYPE_ERROR);
        } else if (status == MessageIds.TEST_FAILED ||
                   failure.getComparison() != null) {
            super.setFailureType(FAILURE_TYPE_FAILURE);
        }

        // this stack trace has been created by EclipseJUnitThrowableClassifier
        // and has the structure we need
        this.content = failure.getTrace();
    }

    @Override
    public void writeFailure(StringBuffer buffer) {
        buffer.append(super.getFailureTypeString());
        if (!this.content.startsWith(":")) {
            buffer.append('\n');
        }
        buffer.append(this.content);
    }
}
