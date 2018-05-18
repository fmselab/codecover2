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

import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: TestSessionSetTestSession.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TestSessionSetTestSession extends MatchingItem {

    /**
     * Gets all the {@link TestSession} that match the given
     * {@link TestSessionContainer}
     * 
     * @param container
     *                the given {@link TestSessionContainer}
     * @return the {@link Set} of matching {@link TestSession}.
     */
    public Set<TestSession> getSessionMatches(TestSessionContainer container) {
        prepare();

        final Set<TestSession> result = new HashSet<TestSession>();
        for (TestSession testSession : container.getTestSessions()) {
            if (evaluate(testSession.getName())) {
                result.add(testSession);
            }
        }
        return result;
    }
}
