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

import java.util.HashSet;
import java.util.Set;

import org.codecover.model.TestCase;
import org.codecover.model.TestSession;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: TestCaseSetTestCase.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TestCaseSetTestCase extends MatchingItem {

    /**
     * Gets all the {@link TestCase} that match the given {@link TestSession}
     * 
     * @param session
     *                the given {@link TestSession}
     * @return the {@link Set} of matching {@link TestCase}.
     */
    public Set<TestCase> getMatches(TestSession session) {
        prepare();

        final Set<TestCase> result = new HashSet<TestCase>();
        for (TestCase testCase : session.getTestCases()) {
            if (evaluate(testCase.getName())) {
                result.add(testCase);
            }
        }
        return result;
    }
}
