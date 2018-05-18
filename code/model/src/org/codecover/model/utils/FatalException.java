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

package org.codecover.model.utils;

/**
 * This exception will be thrown when a message with a {@link LogLevel} of
 * {@link LogLevel#FATAL FATAL} is being logged.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: FatalException.java 185 2013-05-20 19:08:39Z langaujs $)
 */
public final class FatalException extends RuntimeException {
    private static final long serialVersionUID = -1897050574873975808L;

    private final LogMessage logMessage;

    FatalException(LogMessage logMessage) {
        if (logMessage == null) {
            throw new NullPointerException("logMessage == null");
        }

        if (logMessage.getLevel() != LogLevel.FATAL) {
            throw new IllegalArgumentException(
                    "logMessage.getLevel() != LogLevel.FATAL");
        }

        this.logMessage = logMessage;

        initCause(logMessage.getException());
    }

    /**
     * Gets the message contained in the {@link LogMessage}.
     * 
     * @return the message of the {@link LogMessage}.
     */
    @Override
    public String getMessage() {
        return "A fatal error occured: " + this.logMessage.getMessage();
    }
}
