/*
 * Created on 21/11/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class NormalAnnotationExpr extends AnnotationExpr {

    public final NameExpr name;

    public final List<MemberValuePair> pairs;

    public NormalAnnotationExpr(int line, int column, NameExpr name, List<MemberValuePair> pairs) {
        super(line, column);
        this.name = name;
        this.pairs = pairs;
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
