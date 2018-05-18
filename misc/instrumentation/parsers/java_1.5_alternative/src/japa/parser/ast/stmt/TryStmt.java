/*
 * Created on 18/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class TryStmt extends Statement {

    public final BlockStmt tryBlock;

    public final List<CatchClause> catchs;

    public final BlockStmt finallyBlock;

    public TryStmt(int line, int column, BlockStmt tryBlock, List<CatchClause> catchs, BlockStmt finallyBlock) {
        super(line, column);
        this.tryBlock = tryBlock;
        this.catchs = catchs;
        this.finallyBlock = finallyBlock;
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
