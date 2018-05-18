/*
 * Created on 20/01/2007
 */
package japa.parser.ast.body;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class EmptyTypeDeclaration extends TypeDeclaration {

    public EmptyTypeDeclaration(int line, int column) {
        super(line, column, null, 0, null);
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
