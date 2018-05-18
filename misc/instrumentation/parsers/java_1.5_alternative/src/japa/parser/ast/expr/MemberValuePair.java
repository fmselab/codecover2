/*
 * Created on 21/11/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.Node;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class MemberValuePair extends Node {

    public final String name;

    public final Expression value;

    public MemberValuePair(int line, int column, String name, Expression value) {
        super(line, column);
        this.name = name;
        this.value = value;
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
