/*
 * Created on 18/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class SynchronizedStmt extends Statement {

    public final Expression expr;

    public final BlockStmt block;

    public SynchronizedStmt(int line, int column, Expression expr, BlockStmt block) {
        super(line, column);
        this.expr = expr;
        this.block = block;
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
