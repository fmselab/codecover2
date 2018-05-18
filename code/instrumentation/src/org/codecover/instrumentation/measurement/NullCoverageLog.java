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
package org.codecover.instrumentation.measurement;

/**
 * An implementation of {@link CoverageResultLog}, that is doing nothing.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: NullCoverageLog.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class NullCoverageLog implements CoverageResultLog {
    /**
     * An instance of {@link NullCoverageLog} for multiple usage.
     */
    public static final NullCoverageLog INSTANCE = new NullCoverageLog();

    public void startNamedSection(String sectionName) {
        // just implement method
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName) {
        // just implement method
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, long timestamp) {
        // just implement method
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, long timestamp, String comment) {
        // just implement method
    }

    public void startTestCase(String testSessionContainerUID,
            String testCaseName, String comment) {
        // just implement method
    }

    public void endTestCase(String testCaseName) {
        // just implement method
    }

    public void endTestCase(String testCaseName, long timestamp) {
        // just implement method
    }

    public void endTestCase(String testCaseName, long timestamp,
            String resultComment) {
        // just implement method
    }

    public void lineComment(String comment) {
        // just implement method
    }

    public void startLog() {
        // just implement method
    }

    public void closeLog() {
        // just implement method
    }

    public void passCounter(String counterID, long counterValue) {
        // just implement method
    }
}
