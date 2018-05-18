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

package org.codecover.instrumentation;

import java.io.Writer;

/**
 * A {@link Writer}, that does not write anything but consumes all characters.<br>
 * <br>
 * This {@link NullWriter} can for example be used for pretend modes.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: NullWriter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class NullWriter extends Writer {

    /** An instance of {@link NullWriter} */
    public static final NullWriter INSTANCE = new NullWriter();

    @Override
    public void close() {
        // do nothing
    }

    @Override
    public void flush() {
        // do nothing
    }

    @Override
    public void write(char[] cbuf, int off, int len) {
        // do nothing
    }

    @Override
    public void write(char[] cbuf) {
        // do nothing
    }

    @Override
    public void write(int c) {
        // do nothing
    }

    @Override
    public void write(String str) {
        // do nothing
    }

    @Override
    public void write(String str, int off, int len) {
        // do nothing
    }
}
