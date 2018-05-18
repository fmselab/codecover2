/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.batch.exceptions;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: ParseException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ParseException extends Exception {
    private static final long serialVersionUID = -8241389872878224438L;

    /**
     * Construct a new ParseException object with <code>null</code> as its
     * detail message.
     */
    public ParseException() {
        super();
    }

    /**
     * Construct a new ParseException object with <code>message</code> as its
     * detail message.
     * 
     * @param message
     *            The message for this exception.
     */
    public ParseException(String message) {
        super(message);
    }

    /**
     * Constructs a new ParseException with the specified cause and a detail
     * message of <code>(cause==null ? null : cause.toString())</code>
     * 
     * @param cause
     *            The cause of this exception.
     */
    public ParseException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a new ParseException with the specified <code>message</code>
     * and <code>cause</code>.
     * 
     * @param message
     *            The message for this exception.
     * @param cause
     *            The cause of this exception.
     */
    public ParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
