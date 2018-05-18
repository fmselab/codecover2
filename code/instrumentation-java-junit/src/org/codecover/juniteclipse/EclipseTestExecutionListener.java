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

import org.codecover.juniteclipse.runner.EclipseTestRunner;
import org.eclipse.jdt.internal.junit.runner.FirstRunExecutionListener;
import org.eclipse.jdt.internal.junit.runner.IListensToTestExecutions;
import org.eclipse.jdt.internal.junit.runner.ITestIdentifier;
import org.eclipse.jdt.internal.junit.runner.RerunExecutionListener;
import org.eclipse.jdt.internal.junit.runner.TestExecution;
import org.eclipse.jdt.internal.junit.runner.TestIdMap;
import org.eclipse.jdt.internal.junit.runner.TestReferenceFailure;

/**
 * Encapsulates a {@link FirstRunExecutionListener} or a
 * {@link RerunExecutionListener} and a {@link EclipseJUnitListenerMethods}.<br>
 * <br>
 * We need this encapsulation because we have to get to know the starts and ends
 * of the test cases and the {@link TestExecution} allows just one
 * {@link IListensToTestExecutions}. So this listener informs the original
 * listener and our listener too.
 * 
 * @see EclipseTestRunner#firstRunExecutionListener()
 * @see EclipseTestRunner#rerunExecutionListener()
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: EclipseTestExecutionListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipseTestExecutionListener extends RerunExecutionListener {
    private final FirstRunExecutionListener delegateListener;
    private final IListensToTestExecutions codecoverListener;

    /**
     * Constructor.
     * 
     * @param superListener The original listener to be informed.
     * @param codecoverListener The {@link EclipseJUnitListenerMethods} to be informed.
     */
    public EclipseTestExecutionListener(FirstRunExecutionListener superListener,
            IListensToTestExecutions codecoverListener) {
        super(null, new TestIdMap());
        this.delegateListener = superListener;
        this.codecoverListener = codecoverListener;
    }

    @Override
    public void notifyTestStarted(ITestIdentifier test) {
        this.codecoverListener.notifyTestStarted(test);
        this.delegateListener.notifyTestStarted(test);
    }

    @Override
    public void notifyTestEnded(ITestIdentifier test) {
        this.codecoverListener.notifyTestEnded(test);
        this.delegateListener.notifyTestEnded(test);
    }

    @Override
    public void notifyTestFailed(TestReferenceFailure failure) {
        this.codecoverListener.notifyTestFailed(failure);
        this.delegateListener.notifyTestFailed(failure);
    }

    @Override
    public String getStatus() {
        // we expect, that this method is only called, if the super listener
        // was a RerunExecutionListener
        return ((RerunExecutionListener) this.delegateListener).getStatus();
    }
}
