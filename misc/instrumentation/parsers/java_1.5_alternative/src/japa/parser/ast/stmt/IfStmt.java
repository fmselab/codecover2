/*
 * Created on 07/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class IfStmt extends Statement {

    public final Expression condition;

    public final Statement thenStmt;

    public final Statement elseStmt;

    public IfStmt(int line, int column, Expression condition, Statement thenStmt, Statement elseStmt) {
        super(line, column);
        this.condition = condition;
        this.thenStmt = thenStmt;
        this.elseStmt = elseStmt;
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
