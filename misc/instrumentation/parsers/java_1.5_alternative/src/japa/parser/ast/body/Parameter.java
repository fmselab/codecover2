/*
 * Created on 03/11/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.Node;
import japa.parser.ast.expr.AnnotationExpr;
import japa.parser.ast.type.Type;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class Parameter extends Node {

    public final int modifiers;

    public final List<AnnotationExpr> annotations;

    public final Type type;

    public final boolean isVarArgs;

    public final VariableDeclaratorId id;

    public Parameter(int line, int column, int modifiers, List<AnnotationExpr> annotations, Type type, boolean isVarArgs, VariableDeclaratorId id) {
        super(line, column);
        this.modifiers = modifiers;
        this.annotations = annotations;
        this.type = type;
        this.isVarArgs = isVarArgs;
        this.id = id;
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
