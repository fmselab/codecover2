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

/**
 * This class is a parent class for ParseExceptions. It is needed for throwing
 * parse exception out of {@link Instrumenter#instrument(java.io.File, java.io.File, java.util.Collection, org.codecover.model.MASTBuilder, java.util.Map)}. 
 * 
 * The parser's ParseException class must be a child of this class.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: ParseException.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 * @see org.codecover.instrumentation.measurement.parser.ParseException
 */
public class ParseException extends InstrumentationException {

    /**
     * Construct a new ParseException object with <code>null</code> as 
     * its detail message.
     */
    public ParseException() {
        super();
    }

    /**
     * Construct a new ParseException object with <code>message</code> as 
     * its detail message.
     * 
     * @param message the message
     */
    public ParseException(String message) {
        super(message);
    }
}
