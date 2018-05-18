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

package org.codecover.model;

import org.xml.sax.InputSource;

/**
 * A custom input source to use in the {@link SAXFileWriter}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: TestSessionContainerInputSource.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TestSessionContainerInputSource extends InputSource {
    private final TestSessionContainer testSessionContainer;

    /**
     * Constructor
     * 
     * @param testSessionContainer
     *            the {@link TestSessionContainer} to write to a file.
     */
    public TestSessionContainerInputSource(
            TestSessionContainer testSessionContainer) {
        super();
        this.testSessionContainer = testSessionContainer;
    }

    /**
     * Gets the testSessionContainer.
     * 
     * @return the testSessionContainer
     */
    public final TestSessionContainer getTestSessionContainer() {
        return this.testSessionContainer;
    }
}
