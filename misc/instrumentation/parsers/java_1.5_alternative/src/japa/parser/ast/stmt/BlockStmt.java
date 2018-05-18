/*
 * Created on 04/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class BlockStmt extends Statement {

    public final List<Statement> stmts;

    public BlockStmt(int line, int column, List<Statement> stmts) {
        super(line, column);
        this.stmts = stmts;
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
