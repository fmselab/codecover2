/*
 * Created on 05/10/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.Node;
import japa.parser.ast.expr.Expression;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class VariableDeclarator extends Node {

    public final VariableDeclaratorId id;

    public final Expression init;

    public VariableDeclarator(int line, int column, VariableDeclaratorId id, Expression init) {
        super(line, column);
        this.id = id;
        this.init = init;
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
