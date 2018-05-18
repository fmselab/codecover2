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

import java.util.*;

import org.codecover.batch.Option;
import org.codecover.model.utils.*;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: MissingOptionException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MissingOptionException extends ParseException {
    private static final long serialVersionUID = 199562345468928555L;

    private final Set<Option> options;

    /**
     * Construct a new MissingOptionException object with <code>message</code>
     * as its detail message.
     * 
     * @param options
     *            the {@link Option}s
     */
    public MissingOptionException(Set<Option> options) {
        super();
        this.options = CollectionUtil.copy(options);
    }

    /**
     * Gets the {@link Option}s
     * 
     * @return the {@link Option}s
     */
    public Set<Option> getOptions() {
        return this.options;
    }
}
