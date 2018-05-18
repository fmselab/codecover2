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

package org.codecover.model.exceptions;

/**
 * 
 * @author Markus
 * @version 1.0 ($Id: MergeException.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public class MergeException extends ModelException {
    private static final long serialVersionUID = 2458748560751205737L;

    /**
     * Construct a new MergeException object with <code>message</code> as
     * its detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public MergeException(String message) {
        super(message);
    }

    /**
     * Constructs a new MergeException with the specified
     * <code>message</code> and <code>cause</code>.
     * 
     * @param message
     *            The message for this exception.
     * @param cause
     *            The cause of this exception.
     */
    public MergeException(String message, Throwable cause) {
        super(message, cause);
    }
}
