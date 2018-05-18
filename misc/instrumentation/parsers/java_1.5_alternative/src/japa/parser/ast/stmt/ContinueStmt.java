/*
 * Created on 07/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class ContinueStmt extends Statement {

    public final String id;

    public ContinueStmt(int line, int column, String id) {
        super(line, column);
        this.id = id;
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
