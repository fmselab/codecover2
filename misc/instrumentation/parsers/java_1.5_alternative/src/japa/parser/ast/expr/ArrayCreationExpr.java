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
public final class ArrayCreationExpr extends Expression {

    public final Type type;

    public final List<Type> typeArgs;

    public final int arrayCount;

    public final ArrayInitializerExpr initializer;

    public final List<Expression> dimensions;

    public ArrayCreationExpr(int line, int column, Type type, List<Type> typeArgs, int arrayCount, ArrayInitializerExpr initializer) {
        super(line, column);
        this.type = type;
        this.typeArgs = typeArgs;
        this.arrayCount = arrayCount;
        this.initializer = initializer;
        this.dimensions = null;
    }

    public ArrayCreationExpr(int line, int column, Type type, List<Type> typeArgs, List<Expression> dimensions, int arrayCount) {
        super(line, column);
        this.type = type;
        this.typeArgs = typeArgs;
        this.arrayCount = arrayCount;
        this.dimensions = dimensions;
        this.initializer = null;
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
