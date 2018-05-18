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

package org.codecover.instrumentation.java15.manipulators;

import java.io.IOException;

import org.codecover.instrumentation.java.measurement.Protocol;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;

/**
 * The {@link CommentManipulator} is responsible for parsing
 * <code>// startTestCase("Name");</code> in comments and creating a start
 * statement of the {@link Protocol} ({@link Protocol#startTestCase(String)}).
 * 
 * @see DefaultCommentManipulator
 * @see DummyCommentManipulator
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: CommentManipulator.java 22 2008-05-25 20:08:53Z ahija $)
 */
public interface CommentManipulator extends Manipulator {
    /**
     * Tries to find special comments in the special {@link NodeToken}.
     * 
     * @param token The special {@link NodeToken}.
     * 
     * @return true &rarr; this method has manipulated the {@link NodeToken} 
     * and written it to the writer; false &rarr; the {@link NodeToken} has not been
     * written yet &rarr; write it.
     * 
     * @throws IOException If there occurs an error during writing. 
     */
    public boolean manipulate(NodeToken token) throws IOException;
}
