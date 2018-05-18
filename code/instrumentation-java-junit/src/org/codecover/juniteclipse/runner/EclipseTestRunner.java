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
package org.codecover.juniteclipse.runner;

import org.codecover.juniteclipse.EclipseJUnitListenerMethods;
import org.codecover.juniteclipse.EclipseJUnitThrowableClassifier;
import org.codecover.juniteclipse.EclipseTestExecutionListener;
import org.eclipse.jdt.internal.junit.runner.FirstRunExecutionListener;
import org.eclipse.jdt.internal.junit.runner.IClassifiesThrowables;
import org.eclipse.jdt.internal.junit.runner.RemoteTestRunner;
import org.eclipse.jdt.internal.junit.runner.RerunExecutionListener;

/**
 * Extends a {@link RemoteTestRunner} to add special
 * {@link EclipseTestExecutionListener}, which are needed to protocol the start
 * and the end of test cases.
 * 
 * @see RemoteTestRunner
 * @see EclipseJUnitListenerMethods
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: EclipseTestRunner.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipseTestRunner extends RemoteTestRunner {
    /**
     * @see RemoteTestRunner#main(String[])
     */
    public static void main(String[] args) {
        try {
            EclipseTestRunner testRunServer = new EclipseTestRunner();
            testRunServer.init(args);
            testRunServer.run();
        } catch (Throwable e) {
            e.printStackTrace(); // don't allow System.exit(0) to swallow
                                 // exceptions
        } finally {
            // fix for 14434
            System.exit(0);
        }
    }

    private final EclipseJUnitListenerMethods eclipseJUnitListener;

    private EclipseTestRunner() {
        this.eclipseJUnitListener = new EclipseJUnitListenerMethods();
    }

    @Override
    public FirstRunExecutionListener firstRunExecutionListener() {
        FirstRunExecutionListener listener = super.firstRunExecutionListener();
        listener = new EclipseTestExecutionListener(listener,
                this.eclipseJUnitListener);
        return listener;
    }

    @Override
    public RerunExecutionListener rerunExecutionListener() {
        RerunExecutionListener listener = super.rerunExecutionListener();
        listener = new EclipseTestExecutionListener(listener,
                this.eclipseJUnitListener);
        return listener;
    }

    @Override
    protected IClassifiesThrowables getClassifier() {
        return new EclipseJUnitThrowableClassifier(super.getClassifier());
    }
}
