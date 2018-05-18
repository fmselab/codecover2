/*
 * Created on 05/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class UnaryExpr extends Expression {

    public static enum Operator {
        positive, // +
        negative, // -
        preIncrement, // ++
        preDecrement, // --
        not, // !
        inverse, // ~
        posIncrement, // ++
        posDecrement, // --
    }

    public final Expression expr;

    public final Operator op;

    public UnaryExpr(int line, int column, Expression expr, Operator op) {
        super(line, column);
        this.expr = expr;
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
