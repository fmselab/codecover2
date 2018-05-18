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

package org.codecover.report.exceptions;

/**
 * An exception class used for signaling failure of creating a file.
 *
 * @author Michael Starzmann
 * @version 1.0 ($Id: FileCreationException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class FileCreationException extends ReportException {

    private static final long serialVersionUID = 0L;

    /**
     * Constructs a new exception with the specified cause and a detail
     * message of <tt>(cause==null ? null : cause.toString())</tt> (which
     * typically contains the class and detail message of <tt>cause</tt>).
     * This constructor is useful for exceptions that are little more than
     * wrappers for other throwables (for example, {@link
     * java.security.PrivilegedActionException}).
     *
     * @param  cause the cause (which is saved for later retrieval by the
     *         {@link #getCause()} method).  (A <tt>null</tt> value is
     *         permitted, and indicates that the cause is nonexistent or
     *         unknown.)
     */
    public FileCreationException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new exception with the specified detail message.  The
     * cause is not initialized, and may subsequently be initialized by
     * a call to {@link Throwable#initCause}.
     *
     * @param   message   the detail message. The detail message is saved for
     *          later retrieval by the {@link #getMessage()} method.
     */
    public FileCreationException(String message) {
        super(message);
    }

    /**
     * Constructs a ReportException with a given message and a
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param message   the detail (error) message
     * @param cause     the <code>Throwable</code> that caused this exception to
     *                  occur
     */
    public FileCreationException(String message, Throwable cause) {
        super(message, cause);
    }
}
