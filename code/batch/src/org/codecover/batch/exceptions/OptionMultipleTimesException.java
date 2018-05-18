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

import org.codecover.batch.Option;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: OptionMultipleTimesException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class OptionMultipleTimesException extends ParseException {
    private static final long serialVersionUID = 8397806440638140849L;

    private final Option option;

    /**
     * Construct a new OptionMultipleTimesException object with
     * <code>null</code> as its detail message.
     * 
     * @param option
     *            the given {@link Option}
     */
    public OptionMultipleTimesException(Option option) {
        super();
        this.option = option;
    }

    /**
     * Gets the {@link Option}
     * 
     * @return the {@link Option}
     */
    public Option getOption() {
        return this.option;
    }
}
