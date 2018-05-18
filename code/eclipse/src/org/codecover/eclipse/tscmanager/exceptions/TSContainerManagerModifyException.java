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

/**
 * This exception is thrown if a listener tries to modify the
 * <code>TSContainerManager</code> during a change event.
 *  
 * @author Robert Hanussek
 * @version 1.0 ($Id: TSContainerManagerModifyException.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
@SuppressWarnings("serial")
public class TSContainerManagerModifyException extends RuntimeException {
    
    public TSContainerManagerModifyException(String message) {
        super(message);
    }
    
    public TSContainerManagerModifyException(String message,
            Throwable cause) {
        super(message, cause);
    }

}
