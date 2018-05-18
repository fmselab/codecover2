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

import javax.management.MBeanException;

/**
 * An exception to wrap a {@link MBeanException}
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: LiveNotificationMBeanException.java 1673 2007-07-21
 *          10:29:47Z wittlims $)
 */
public class LiveNotificationMBeanException extends LiveNotificationException {

    private static final long serialVersionUID = 5693693615162138899L;

    /**
     * Constructor with a given {@link Throwable} cause
     * 
     * @param message
     *            the message
     * @param cause
     *            the cause
     */
    public LiveNotificationMBeanException(String message, Throwable cause) {
        super(message, cause);
    }
}
