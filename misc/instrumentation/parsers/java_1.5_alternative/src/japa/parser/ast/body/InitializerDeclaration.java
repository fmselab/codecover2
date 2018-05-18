/*
 * Created on 07/11/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.stmt.BlockStmt;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class InitializerDeclaration extends BodyDeclaration {

    public final BlockStmt block;

    public InitializerDeclaration(int line, int column, BlockStmt block) {
        super(line, column);
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
