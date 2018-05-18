/*
 * Created on 05/10/2006
 */
package japa.parser.ast;

import japa.parser.ast.visitor.DumpVisitor;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public abstract class Node {

    private final int line;

    private final int column;

    /**
     * This attribute can store additional information from semantic analysis.
     */
    public Object data;

    public Node(int line, int column) {
        this.line = line;
        this.column = column;
    }

    public <A> void accept(VoidVisitor<A> v, A arg) {
        v.visit(this, arg);
    }

    public <R, A> R accept(GenericVisitor<R, A> v, A arg) {
        return v.visit(this, arg);
    }

    @Override
    public final String toString() {
        DumpVisitor visitor = new DumpVisitor();
        accept(visitor, null);
        return visitor.getSource();
    }

    public final int getLine() {
        return line;
    }

    public final int getColumn() {
        return column;
    }

}
