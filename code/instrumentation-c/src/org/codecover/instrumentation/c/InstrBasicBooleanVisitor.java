package org.codecover.instrumentation.c;

import org.codecover.instrumentation.booleanterms.InstrBasicBooleanTerm;
import org.codecover.instrumentation.c.syntaxtree.Node;
import org.codecover.instrumentation.c.syntaxtree.NodeToken;
import org.codecover.instrumentation.c.visitor.DepthFirstVisitor;

import java.io.StringWriter;

public class InstrBasicBooleanVisitor extends DepthFirstVisitor {
    private StringWriter writer;

    private int foundStartOffset = -1;

    private int foundEndOffset = -1;

    private InstrBasicBooleanVisitor() {
        this.writer = new StringWriter();
    }

    public void visit(NodeToken n) {
        if (n.numSpecials() > 0) {
            for (NodeToken nt : n.specialTokens) {
                this.writer.write(nt.tokenImage);
            }
        }

        this.writer.write(n.tokenImage);
        if (this.foundStartOffset == -1) {
            this.foundStartOffset = n.beginOffset;
        }
        this.foundEndOffset = n.endOffset;
    }

    public static InstrBasicBooleanTerm convertToInstrBasicBoolean(Node n) {
        InstrBasicBooleanVisitor treeStringDumper = new InstrBasicBooleanVisitor();
        n.accept(treeStringDumper);

        return new InstrBasicBooleanTerm(treeStringDumper.writer.toString().trim(),
                treeStringDumper.foundStartOffset,
                treeStringDumper.foundEndOffset);
    }
}
