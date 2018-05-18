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
 * An {@code enum} indicating the importance of a {@link LogMessage}.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: LogLevel.java 185 2013-05-20 19:08:39Z langaujs $)
 */
public enum LogLevel {
    /**
     * Indicates a fatal condition which will cause the current operation
     * to be aborted.
     * 
     * Logging a message with this {@code LogLevel} will cause a
     * {@link FatalException} to be thrown.
     */
    FATAL,
    /**
     * Indicates an error condition where the current operation still can
     * continue.
     */
    ERROR,
    /**
     * Indicates a condition the program wants to warn the user about.
     */
    WARNING,
    /**
     * An information which might be interesting for the user.
     */
    INFO,
    /**
     * An information which probably is not interesting for the end user but
     * for the programmer when he is hunting bugs.
     */
    DEBUG
}
