/*
 * Created on 05/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.type.Type;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class FieldAccessExpr extends Expression {

    public final Expression object;

    public final List<Type> typeArgs;

    public final String field;

    public FieldAccessExpr(int line, int column, Expression object, List<Type> typeArgs, String field) {
        super(line, column);
        this.object = object;
        this.typeArgs = typeArgs;
        this.field = field;
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
