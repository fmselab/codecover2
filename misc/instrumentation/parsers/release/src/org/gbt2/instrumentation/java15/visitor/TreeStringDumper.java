///////////////////////////////////////////////////////////////////////////////
//
// $Id: TreeStringDumper.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 24.02.2007 17:30:00
//
///////////////////////////////////////////////////////////////////////////////

//
// Generated by JTB 1.3.2
//
package org.gbt2.instrumentation.java15.visitor;

import java.io.StringWriter;

import org.gbt2.instrumentation.java15.syntaxtree.NodeToken;

/**
 * Dumps the syntax tree to a Writer using the location information in each
 * NodeToken.
 */
public class TreeStringDumper extends DepthFirstVisitor {
    private StringWriter writer = new StringWriter();

    private int curLine = 1;

    private int curColumn = 1;

    private boolean startAtNextToken = false;

    /**
     * The default constructor uses System.out as its output location. You may
     * specify your own Writer or OutputStream using one of the other
     * constructors.
     */
    public TreeStringDumper() {
        reset();
    }

    public void reset() {
        this.writer = new StringWriter();
        this.startAtNextToken = true;
    }

    public String getContent() {
        return this.writer.toString().trim();
    }

    /**
     * Dumps the current NodeToken to the output stream being used.
     * 
     * @throws IllegalStateException
     *             if the token position is invalid relative to the current
     *             position, i.e. its location places it before the previous
     *             token.
     */
    @Override
    public void visit(NodeToken n) {
        if (n.beginLine == -1 || n.beginColumn == -1) {
            printToken(n.tokenImage);
            return;
        }

        //
        // Handle special tokens
        //
        if (n.numSpecials() > 0) {
            for (NodeToken nt : n.specialTokens) {
                visit(nt);
            }
        }

        //
        // Handle startAtNextToken option
        //
        if (this.startAtNextToken) {
            this.curLine = n.beginLine;
            this.curColumn = n.beginColumn;
            this.startAtNextToken = false;
        }

        //
        // Move output "cursor" to proper location, then print the token
        //
        if (this.curLine < n.beginLine) {
            this.curColumn = 1;
            for (; this.curLine < n.beginLine; ++this.curLine) {
                this.writer.write('\n');
            }
        }

        for (; this.curColumn < n.beginColumn; ++this.curColumn) {
            this.writer.write(' ');
        }

        printToken(n.tokenImage);
    }

    private void printToken(String s) {
        for (int i = 0; i < s.length(); ++i) {
            if (s.charAt(i) == '\n') {
                ++this.curLine;
                this.curColumn = 1;
            } else {
                this.curColumn++;
            }

            this.writer.write(s.charAt(i));
        }
    }
}
