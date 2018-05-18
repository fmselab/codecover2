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

import java.io.File;
import java.io.FileNotFoundException;

import org.codecover.instrumentation.Instrumenter;

/**
 * This is a special {@link InstrumentationException} used within
 * {@link Instrumenter}.<br>
 * <br>
 * It is used to inform the caller of the Instrumenter of an
 * {@link FileNotFoundException}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumentationFileNotFoundException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumentationFileNotFoundException extends
        InstrumentationException {

    /**
     * Constructs a new InstrumentationFileNotFoundException with the specified
     * {@link FileNotFoundException} as <code>cause</code>.
     * 
     * @param cause
     *            The cause of this exception.
     */
    public InstrumentationFileNotFoundException(FileNotFoundException cause) {
        super(cause.getMessage(), cause);
    }

    /**
     * Constructs a new InstrumentationFileNotFoundException saying that 
     * as <code>fileWhichIsNotFound</code> is not found.
     *
     * @param fileWhichIsNotFound
     *          The file that was not found, but expected.
     */
    public InstrumentationFileNotFoundException(File fileWhichIsNotFound) {
        this(new FileNotFoundException(fileWhichIsNotFound + " was not found"));
        
    }
}
