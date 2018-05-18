/*
 * Created on 05/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.body.BodyDeclaration;
import japa.parser.ast.type.ClassOrInterfaceType;
import japa.parser.ast.type.Type;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class ObjectCreationExpr extends Expression {

    public final Expression scope;

    public final ClassOrInterfaceType type;

    public final List<Type> typeArgs;

    public final List<Expression> args;

    public final List<BodyDeclaration> anonymousClassBody;

    public ObjectCreationExpr(int line, int column, Expression scope, ClassOrInterfaceType type, List<Type> typeArgs, List<Expression> args, List<BodyDeclaration> anonymousBody) {
        super(line, column);
        this.scope = scope;
        this.type = type;
        this.typeArgs = typeArgs;
        this.args = args;
        this.anonymousClassBody = anonymousBody;
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
