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

package org.codecover.instrumentation.cobol85.visitor;

import java.io.PrintWriter;
import java.io.Writer;
import java.util.Locale;

import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;

/**
 * A modified writer for instrumentation purposes. Handles possible white spaces
 * at the beginning of each line of code.
 * 
 * @author Stefan Franke
 * @version 1.0 ($Id: InstPrintWriter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstPrintWriter extends java.io.PrintWriter {

    private CompilerDirectivesManipulator compilerDirectivesManipulator;

    /**
     * Constructs a new {@link InstPrintWriter}.
     *
     * @param out the output writer
     * @param autoFlush do auto flush
     * @param compilerDirectivesManipulator the compiler directive manipulator
     */
    public InstPrintWriter(Writer out, boolean autoFlush,
            CompilerDirectivesManipulator compilerDirectivesManipulator) {
        super(out, autoFlush);
        this.compilerDirectivesManipulator = compilerDirectivesManipulator;
    }

    @Override
    public PrintWriter printf(String format, Object... args) {
        //String s = this.compilerDirectivesManipulator.insertWhiteSpace(format);
        return super.printf(format, args);
    }
    
    @Override
    public PrintWriter printf(Locale l, String format, Object... args) {
        //String s = this.compilerDirectivesManipulator.insertWhiteSpace(format);
        return super.printf(l, format, args);
    }

    @Override
    public void print(String s) {
        //String str = this.compilerDirectivesManipulator.insertWhiteSpace(s);
        super.print(s);
    }

    @Override
    public void println(String s) {
        //String str = this.compilerDirectivesManipulator.insertWhiteSpace(s);
        super.println(s);
    }
}
