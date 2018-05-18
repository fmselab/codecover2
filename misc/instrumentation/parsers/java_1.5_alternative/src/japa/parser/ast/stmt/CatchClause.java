/*
 * Created on 18/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.Node;
import japa.parser.ast.body.Parameter;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class CatchClause extends Node {

    public final Parameter except;

    public final BlockStmt catchBlock;

    public CatchClause(int line, int column, Parameter except, BlockStmt catchBlock) {
        super(line, column);
        this.except = except;
        this.catchBlock = catchBlock;
    }

    @Override
    public <A> void accept(VoidVisitor<A> v, A arg) {
        v.visit(this, arg);
    }

    @Override
    public <R, A> R accept(GenericVisitor<R, A> v, A arg) {
        return v.visit(this, arg);
    }
}
