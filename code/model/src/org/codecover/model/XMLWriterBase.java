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

package org.codecover.model;

import org.codecover.model.utils.Logger;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: XMLWriterBase.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
abstract class XMLWriterBase implements XMLNames {
    private final Logger logger;

    XMLWriterBase(Logger logger) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }

        this.logger = logger;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.model.XMLNames#getLogger()
     */
    public Logger getLogger() {
        return this.logger;
    }
}
