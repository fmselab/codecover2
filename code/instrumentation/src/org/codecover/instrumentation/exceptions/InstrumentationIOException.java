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

package org.codecover.instrumentation.exceptions;

import java.io.IOException;

import org.codecover.instrumentation.Instrumenter;

/**
 * This is a special {@link InstrumentationException} used within
 * {@link Instrumenter}.<br>
 * <br>
 * It is used to inform the caller of the Instrumenter of an {@link IOException}
 * or other file related abnormalities.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumentationIOException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumentationIOException extends InstrumentationException {

    /**
     * Construct a new InstrumentationIOException object with
     * <code>message</code> as its detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public InstrumentationIOException(String message) {
        super(message);
    }

    /**
     * Constructs a new InstrumentationIOException with the specified
     * <code>message</code> and <code>cause</code>.
     * 
     * @param cause
     *            The cause of this exception.
     */
    public InstrumentationIOException(IOException cause) {
        super(cause.getMessage(), cause);
    }
}
