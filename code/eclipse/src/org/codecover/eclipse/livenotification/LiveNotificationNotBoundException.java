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

package org.codecover.eclipse.livenotification;

import java.rmi.NotBoundException;

/**
 * An exception to wrap {@link NotBoundException}s, that don't represent
 * network errors.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationNotBoundException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LiveNotificationNotBoundException extends
        LiveNotificationException {

    private static final long serialVersionUID = 5139418549312555392L;

    /**
     * Constructor with a given {@link Throwable} cause
     * 
     * @param message
     *            the message
     * @param cause
     *            the cause
     */
    public LiveNotificationNotBoundException(String message, Throwable cause) {
        super(message, cause);
    }
}
