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

import java.rmi.RemoteException;

/**
 * An exception to wrap a {@link RemoteException}
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationRemoteException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LiveNotificationRemoteException extends LiveNotificationException {
    private static final long serialVersionUID = -7933275584248755468L;

    /**
     * Constructor with a given {@link Throwable} cause
     * 
     * @param message
     *            the message
     * @param cause
     *            the cause
     */
    public LiveNotificationRemoteException(String message, Throwable cause) {
        super(message, cause);
    }
}
