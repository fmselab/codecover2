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
 * @version 1.0 ($Id: UnknownOptionException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class UnknownOptionException extends ParseException {
    /**
     * Construct a new UnknownOptionException object with <code>null</code> as
     * its detail message.
     * 
     * @param message
     *            the given message
     */
    public UnknownOptionException(String message) {
        super(message);
    }

    /**
     * Gets the name of the unkown option
     * 
     * @return the name of the unkown option
     */
    public abstract String getOptionName();
}
