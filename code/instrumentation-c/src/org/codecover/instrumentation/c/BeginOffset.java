package org.codecover.instrumentation.c;

import org.codecover.instrumentation.c.syntaxtree.Node;
import org.codecover.instrumentation.c.syntaxtree.NodeToken;
import org.codecover.instrumentation.c.visitor.DepthFirstVisitor;

/**
 * Gets the first {@link NodeToken#beginOffset}
 */
public class BeginOffset extends DepthFirstVisitor {
    private int foundStartOffset = -1;

    private BeginOffset() {
        // private constructor
    }

    /**
     * Assigns {@link #foundStartOffset} and throws an {@link RuntimeException}.
     */
    @Override
    public void visit(NodeToken n) {
        this.foundStartOffset = n.beginOffset;
        throw new RuntimeException();
    }

    /**
     * This method seaches in a {@link Node} for the first
     * {@link NodeToken#beginOffset}. <br>
     * This method calls:
     * <p/>
     * <pre>
     * StartOffset startOffset = new StartOffset();
     *
     * try {
     *     n.accept(startOffset);
     * } catch (RuntimeException e) {
     *     // expected
     * }
     *
     * return startOffset.foundStartOffset;
     * </pre>
     *
     * @param n The Node to start the search from.
     * @return The first {@link NodeToken#beginOffset}. <code>-1</code> if
     *         not present.
     */
    public static int getStartOffset(Node n) {
        BeginOffset beginOffset = new BeginOffset();

        try {
            n.accept(beginOffset);
        } catch (RuntimeException e) {
            // expected
        }

        return beginOffset.foundStartOffset;
    }
}
