/*
 * Created on 05/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class BinaryExpr extends Expression {

    public static enum Operator {
        or, // ||
        and, // &&
        binOr, // |
        binAnd, // &
        xor, // ^
        equals, // ==
        notEquals, // !=
        less, // <
        greater, // >
        lessEquals, // <=
        greaterEquals, // >=
        lShift, // <<
        rSignedShift, // >>
        rUnsignedShift, // >>>
        plus, // +
        minus, // -
        times, // *
        divide, // /
        remainder, // %
    }

    public final Expression left;

    public final Expression right;

    public final Operator op;

    public BinaryExpr(int line, int column, Expression left, Expression right, Operator op) {
        super(line, column);
        this.left = left;
        this.right = right;
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
