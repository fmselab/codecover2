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
 * @version 1.0 ($Id: UnknownLongOptionException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UnknownLongOptionException extends UnknownOptionException {
    private static final long serialVersionUID = -7721410320496879978L;

    private final String option;

    /**
     * Construct a new UnknownLongOptionException object with <code>null</code>
     * as its detail message.
     * 
     * @param option
     *            the given option
     */
    public UnknownLongOptionException(String option) {
        super("Unknown option --" + option);
        this.option = option;
    }

    /**
     * Gets the option
     * 
     * @return the option
     */
    public String getOption() {
        return this.option;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.batch.exceptions.UnknownOptionException#getOptionName()
     */
    @Override
    public String getOptionName() {
        return "--" + getOption();
    }
}
