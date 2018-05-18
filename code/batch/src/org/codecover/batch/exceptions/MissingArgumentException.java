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
 * @version 1.0 ($Id: MissingArgumentException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MissingArgumentException extends ParseException {
    private static final long serialVersionUID = -5031304202490907653L;

    private final Option option;

    private final boolean isLongOption;

    /**
     * Construct a new MissingArgumentException object with <code>null</code>
     * as its detail message.
     * 
     * @param option
     *            the {@link Option}
     * @param isLongOption
     *            indicates, if the {@link Option} is a longOption
     */
    public MissingArgumentException(Option option, boolean isLongOption) {
        super();
        this.option = option;
        this.isLongOption = isLongOption;
    }

    /**
     * Gets the {@link Option}
     * 
     * @return the {@link Option}
     */
    public Option getOption() {
        return this.option;
    }

    /**
     * Indicates, whether or not the {@link Option} is a long {@link Option}
     * 
     * @return <code>true</code> &rarr; the {@link Option} is a long
     *         {@link Option} <br>
     *         <code>false</code> &rarr; the {@link Option} is not a long
     *         {@link Option}
     */
    public boolean getIsLongOption() {
        return this.isLongOption;
    }
}
