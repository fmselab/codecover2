/*
 * Created on 05/10/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.Node;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class VariableDeclaratorId extends Node {

    public final String name;

    public final int arrayCount;

    public VariableDeclaratorId(int line, int column, String name, int arrayCount) {
        super(line, column);
        this.name = name;
        this.arrayCount = arrayCount;
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
