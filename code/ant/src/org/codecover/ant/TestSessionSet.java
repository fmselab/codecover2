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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.tools.ant.types.DataType;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: TestSessionSet.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
// TODO: What should we do when an element is referenced with "name=" but
// doesn't exist?
public class TestSessionSet extends DataType {
    private final List<TestSessionSetTestSession> testSessions = new ArrayList<TestSessionSetTestSession>();

    /**
     * Adds a configured {@link TestSessionSetTestSession} to this command.
     * 
     * @param testSession
     *                the {@link TestSessionSetTestSession} to add.
     */
    public void addConfiguredTestSession(TestSessionSetTestSession testSession) {
        this.testSessions.add(testSession);
    }

    /**
     * Gets all the {@link TestSession} that match the given
     * {@link TestSessionContainer}
     * 
     * @param container
     *                the given {@link TestSessionContainer}
     * @return the {@link Set} of matching {@link TestSession}.
     */
    public Set<TestSession> getSessionMatches(TestSessionContainer container) {
        final Set<TestSession> result = new HashSet<TestSession>();
        for (TestSessionSetTestSession childElement : this.testSessions) {
            result.addAll(childElement.getSessionMatches(container));
        }
        return result;
    }
}
