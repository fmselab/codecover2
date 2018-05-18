/*
 * Created on 03/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class ExplicitConstructorInvocationStmt extends Statement {

    public final boolean isThis;

    public final Expression expr;

    public final List<Expression> args;

    public ExplicitConstructorInvocationStmt(int line, int column, boolean isThis, Expression expr, List<Expression> args) {
        super(line, column);
        this.isThis = isThis;
        this.expr = expr;
        this.args = args;
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
