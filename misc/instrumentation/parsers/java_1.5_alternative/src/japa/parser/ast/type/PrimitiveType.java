/*
 * Created on 05/10/2006
 */
package japa.parser.ast.type;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class PrimitiveType extends Type {

    public enum Primitive {
        Boolean, Char, Byte, Short, Int, Long, Float, Double
    }

    public final Primitive type;

    public PrimitiveType(int line, int column, Primitive type) {
        super(line, column);
        this.type = type;
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
