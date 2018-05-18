/*
 * Created on 04/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class SwitchStmt extends Statement {

    public final Expression selector;

    public final List<SwitchEntryStmt> entries;

    public SwitchStmt(int line, int column, Expression selector, List<SwitchEntryStmt> entries) {
        super(line, column);
        this.selector = selector;
        this.entries = entries;
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
