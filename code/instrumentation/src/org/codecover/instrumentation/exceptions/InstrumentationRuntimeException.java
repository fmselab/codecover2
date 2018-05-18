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

import org.codecover.instrumentation.Instrumenter;
import org.codecover.model.utils.FatalException;

/**
 * This is a special {@link InstrumentationException} used within
 * {@link Instrumenter}.<br>
 * <br>
 * It is used to inform the caller of the Instrumenter of an
 * {@link RuntimeException}, {@link FatalException} or other abnormalities
 * caused by a wrong implementation or wrong inputs of the user.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumentationRuntimeException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumentationRuntimeException extends InstrumentationException {

    /**
     * Construct a new InstrumentationRuntimeException object with
     * <code>message</code> as its detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public InstrumentationRuntimeException(String message) {
        super(message);
    }

    /**
     * Constructs a new InstrumentationRuntimeException with the specified
     * <code>message</code> and <code>cause</code>.
     * 
     * @param cause
     *            The cause of this exception.
     */
    public InstrumentationRuntimeException(RuntimeException cause) {
        super(cause.getMessage(), cause);
    }
}
