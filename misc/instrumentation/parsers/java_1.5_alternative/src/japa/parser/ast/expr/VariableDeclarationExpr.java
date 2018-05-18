/*
 * Created on 03/11/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.body.VariableDeclarator;
import japa.parser.ast.type.Type;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class VariableDeclarationExpr extends Expression {

    public final int modifiers;

    public final List<AnnotationExpr> annotations;

    public final Type type;

    public final List<VariableDeclarator> vars;

    public VariableDeclarationExpr(int line, int column, int modifiers, List<AnnotationExpr> annotations, Type type, List<VariableDeclarator> vars) {
        super(line, column);
        this.modifiers = modifiers;
        this.annotations = annotations;
        this.type = type;
        this.vars = vars;
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
