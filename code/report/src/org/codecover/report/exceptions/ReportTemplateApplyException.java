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
 * An exception class used for signaling failure of applying a successfully
 *  loaded template.
 *
 * @author Robert Hanussek
 * @version 1.0 ($Id: ReportTemplateApplyException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ReportTemplateApplyException extends ReportException {

    private static final long serialVersionUID = 0L;

    /**
     * Constructs a ReportTemplateApplyException with a given message.
     *
     * @param message  the error message
     */
    public ReportTemplateApplyException(String message) {
        super(message);
    }

    /**
     * Constructs a ReportTemplateApplyException with a given
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param cause  the <code>Throwable</code> that caused this exception to
     *               occur
     */
    public ReportTemplateApplyException(Throwable cause) {
        super(cause);
    }

    /**
     * Constructs a ReportTemplateApplyException with a given message and a
     * <code>Throwable</code> that was its underlying cause.
     *
     * @param message   the error message
     * @param cause     the <code>Throwable</code> that caused this exception to
     *                  occur
     */
    public ReportTemplateApplyException(String message, Throwable cause) {
        super(message, cause);
    }
}
