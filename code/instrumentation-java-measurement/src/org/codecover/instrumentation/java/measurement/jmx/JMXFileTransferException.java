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

package org.codecover.instrumentation.java.measurement.jmx;

/**
 * This class wraps an exception and is to be used to represent errors, that
 * occured while transfering the coverage log file.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: JMXFileTransferException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JMXFileTransferException extends Exception {

    /**
     * Constructor with a given {@link Throwable} cause
     * 
     * @param cause
     *            the cause
     */
    public JMXFileTransferException(Throwable cause) {
        super(cause);
    }
}
