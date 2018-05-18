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
 * This is a special {@link Exception} used within the model
 * 
 * @author Markus Wittlinger
 * 
 * @version 1.0 ($Id: ModelException.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class ModelException extends Exception {
    private static final long serialVersionUID = -1953892348880410805L;

    /**
     * Construct a new ModelException object with <code>null</code> as its
     * detail message.
     */
    public ModelException() {
        super();
    }

    /**
     * Construct a new ModelException object with <code>message</code> as its
     * detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public ModelException(String message) {
        super(message);
    }

    /**
     * Constructs a new ModelException with the specified cause and a detail
     * message of <code>(cause==null ? null : cause.toString())</code>
     * 
     * @param cause
     *            The cause of this exception.
     */
    public ModelException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new ModelException with the specified
     * <code>message</code> and <code>cause</code>.
     * 
     * @param message
     *            The message for this exception.
     * @param cause
     *            The cause of this exception.
     */
    public ModelException(String message, Throwable cause) {
        super(message, cause);
    }
}
