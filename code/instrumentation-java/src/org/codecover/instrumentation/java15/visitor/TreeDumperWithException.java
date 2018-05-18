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

// Generated by JTB 1.3.2
package org.codecover.instrumentation.java15.visitor;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.util.LinkedList;

import org.codecover.instrumentation.java15.location.OffsetListener;
import org.codecover.instrumentation.java15.syntaxtree.NodeToken;

/**
 * Dumps the syntax tree to a Writer using {@link NodeToken#getSourceFileImage()}.<br>
 * This class was generated by <code>JTB 1.3.2</code> and was named
 * <code>TreeDumper</code>.<br>
 * In contrast to this <code>TreeDumper</code>, this
 * {@link TreeDumperWithException} uses an ordinary {@link Writer} for output
 * and no {@link PrintWriter}. In consequence all visit and access methods have
 * to be declared as <code>throws IOException.</code>. Thus the
 * {@link DepthFirstVisitorWithException} is extended.
 * 
 * @author Generated by JTB 1.3.2
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: TreeDumperWithException.java 22 2008-05-25 20:08:53Z ahija $)
 */
public class TreeDumperWithException extends DepthFirstVisitorWithException {
    /** The separator for lines: {@value} */
    public static final String LINE_SEPARATOR = "\n";

    private Writer targetWriter;
    
    private LinkedList<OffsetListener> offsetListener;

    private StringBuilder instrumentationBetween = new StringBuilder();

    private int lastEndOffset = 0;

    /**
     * Constructor of a new {@link TreeDumperWithException}.
     * 
     * @param writer
     *            The writer to dump in.
     */
    public TreeDumperWithException(Writer writer) {
        this.targetWriter = writer;
        this.offsetListener = new LinkedList<OffsetListener>();
    }

    /**
     * @return The targetWriter.
     */
    public Writer getTargetWriter() {
        return this.targetWriter;
    }

    /**
     * @return the last {@link NodeToken#endOffset}
     * 
     */
    public int getLastEndOffset() {
        return this.lastEndOffset;
    }

    /**
     * Adds an {@link OffsetListener}, which is informed of the next start
     * offset found of {@link NodeToken#startOffset}.
     * 
     * @param listener The {@link OffsetListener} to be informed.
     */
    public void addOffsetListener(OffsetListener listener) {
        this.offsetListener.addLast(listener);
    }

    /**
     * Before the next <b>non</b>-special Token is written, this String is pushed to the
     * {@link #targetWriter}. More than one Strings can be added - they will be concatenated.
     * 
     * @param instrumentation The instrumentation String, that should be added.
     */
    public void addInstrumentationBetween(String instrumentation) {
        this.instrumentationBetween.append(instrumentation);
    }

    /**
     * Dumps the current NodeToken to the output stream being used.
     * 
     * @throws IOException
     *             When there occurs an exception when writing:
     *             {@link Writer#write(char[])}.
     */
    @Override
    public void visit(NodeToken n) throws IOException {
        if (n.numSpecials() > 0) {
            for (NodeToken nt : n.specialTokens) {
                visitSpecial(nt);
            }
        }

        if (this.instrumentationBetween.length() > 0) {
            // if we have saved some instrumentation Strings, put it out before
            // the non-special Token
            for (int i = 0; i < this.instrumentationBetween.length(); i++) {
                this.targetWriter.write(this.instrumentationBetween.charAt(i));
            }
            this.instrumentationBetween.setLength(0);
        }

        this.targetWriter.write(n.getSourceFileImage());
        this.lastEndOffset = n.endOffset;
        while (!this.offsetListener.isEmpty()) {
            this.offsetListener.poll().startOffset(n.startOffset);
        }
    }

    /**
     * Visits a {@link NodeToken#specialTokens}, which is a {@link NodeToken}
     * itself.
     * 
     * @param n
     *            The special {@link NodeToken}.
     * 
     * @throws IOException
     *             When there occurs an exception when writing:
     *             {@link Writer#write(String)}.
     */
    protected void visitSpecial(NodeToken n) throws IOException {
        this.targetWriter.write(n.getSourceFileImage());
    }
}
