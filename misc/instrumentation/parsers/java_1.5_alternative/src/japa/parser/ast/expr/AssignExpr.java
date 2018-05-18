/*
 * Created on 05/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class AssignExpr extends Expression {

    public static enum Operator {
        assign, // =
        plus, // +=
        minus, // -=
        star, // *=
        slash, // /=
        and, // &=
        or, // |=
        xor, // ^=
        rem, // %=
        lShift, // <<=
        rSignedShift, // >>=
        rUnsignedShift, // >>>=
    }

    public final Expression target;

    public final Expression value;

    public final Operator op;

    public AssignExpr(int line, int column, Expression target, Expression value, Operator op) {
        super(line, column);
        this.target = target;
        this.value = value;
        this.op = op;
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
