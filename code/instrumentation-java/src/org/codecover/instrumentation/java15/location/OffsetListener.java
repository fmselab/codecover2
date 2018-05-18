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
package org.codecover.instrumentation.java15.location;

import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;

/**
 * A listener for {@link TreeDumperWithException#addOffsetListener(OffsetListener)}.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: OffsetListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface OffsetListener {
    
    /**
     * A start offset was found, here it is.
     * 
     * @param startOffset The found start offset.
     */
    public void startOffset(int startOffset);
}
