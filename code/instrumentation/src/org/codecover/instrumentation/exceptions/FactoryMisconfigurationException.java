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

package org.codecover.instrumentation.exceptions;

import org.codecover.instrumentation.InstrumenterFactory;

/**
 * An {@link Exception} caused by a missconfiguration of
 * {@link InstrumenterFactory} and a call of
 * {@link InstrumenterFactory#getInstrumenter()}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: FactoryMisconfigurationException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */

public class FactoryMisconfigurationException extends InstrumentationException {
    /**
     * Constructs a new {@link FactoryMisconfigurationException} with a
     * message.
     * 
     * @param message
     *            The message.
     */
    public FactoryMisconfigurationException(String message) {
        super(message);
    }
}
