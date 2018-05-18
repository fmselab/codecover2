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

package org.codecover.eclipse.tscmanager.exceptions;

import java.util.List;

import org.codecover.model.utils.CollectionUtil;

/**
 * This exception is thrown if an error happened while performing a queued
 * save of a test session container.
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSCQueuedSaveException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("serial")
public class TSCQueuedSaveException extends Exception {

    private final List<Throwable> causes;

    public TSCQueuedSaveException(List<Throwable> causes) {
        super();
        this.causes = CollectionUtil.copy(causes);
    }

    public List<Throwable> getCauses() {
        return this.causes;
    }

}
