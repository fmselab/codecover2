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
 * An exception class used for signaling failure of parsing a report template,
 * that does <em>not</em> include errors while configuring or initializing a
 * parser.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: TemplateParseException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TemplateParseException extends TemplateException {

    private static final long serialVersionUID = 0L;

    /**
     * Constructs a TemplateParseException with a given message.
     *
     * @param message  the error message
     */
    public TemplateParseException(String message) {
        super(message);
    }

    /**
     * Constructs a TemplateParseException with a given <code>Throwable</code>
     * that was its underlying cause.
     *
     * @param cause  the <code>Throwable</code> that caused this exception to
     *               occur
     */
    public TemplateParseException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a TemplateParseException with a given message and a
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param message   the error message
     * @param cause     the <code>Throwable</code> that caused this exception to
     *                  occur
     */
    public TemplateParseException(String message, Throwable cause) {
        super(message, cause);
    }
}
