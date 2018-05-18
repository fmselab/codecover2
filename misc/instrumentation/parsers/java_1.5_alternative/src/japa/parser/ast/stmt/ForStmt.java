/*
 * Created on 07/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class ForStmt extends Statement {

    public final List<Expression> init;

    public final List<Expression> update;

    public final Expression iterable;

    public final Statement body;

    public ForStmt(int line, int column, List<Expression> init, Expression iterable, List<Expression> update, Statement body) {
        super(line, column);
        this.iterable = iterable;
        this.init = init;
        this.update = update;
        this.body = body;
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
