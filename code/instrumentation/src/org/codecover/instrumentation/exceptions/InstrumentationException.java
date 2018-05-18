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

import java.io.File;

import org.codecover.instrumentation.Instrumenter;

/**
 * This is a special {@link Exception} used within {@link Instrumenter}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumentationException.java 1 2007-12-12 17:37:26Z t-scheller $)
 *          
 * @see InstrumentationFileNotFoundException
 * @see InstrumentationIOException
 * @see InstrumentationRuntimeException
 * @see ParseException
 */
public class InstrumentationException extends Exception {

    private File fileOfException;
    
    /**
     * Construct a new InstrumentationException object with <code>null</code>
     * as its detail message.
     */
    public InstrumentationException() {
        super();
    }

    /**
     * Construct a new InstrumentationException object with <code>message</code>
     * as its detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public InstrumentationException(String message) {
        super(message);
    }

    /**
     * Constructs a new InstrumentationException with the specified cause and a
     * detail message of <code>(cause==null ? null : cause.toString())</code>
     * 
     * @param cause
     *            The cause of this exception.
     */
    public InstrumentationException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new InstrumentationException with the specified
     * <code>message</code> and <code>cause</code>.
     * 
     * @param message
     *            The message for this exception.
     * @param cause
     *            The cause of this exception.
     */
    public InstrumentationException(String message, Throwable cause) {
        super(message, cause);
    }

    /**
     * @return The File, where the {@link InstrumentationException} occured. Can
     *         be <code>null</code>.
     */
    public File getFileOfException() {
        return this.fileOfException;
    }

    /**
     * @param fileOfException
     *            The File, where the {@link InstrumentationException} occured.
     */
    public void setFileOfException(File fileOfException) {
        this.fileOfException = fileOfException;
    }
}
