/*
 * Created on 05/10/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.expr.AnnotationExpr;
import japa.parser.ast.type.ClassOrInterfaceType;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class EnumDeclaration extends TypeDeclaration {

    public final List<AnnotationExpr> annotations;

    public final List<ClassOrInterfaceType> implementsList;

    public final List<EnumConstantDeclaration> entries;

    public EnumDeclaration(int line, int column, int modifiers, List<AnnotationExpr> annotations, String name, List<ClassOrInterfaceType> implementsList, List<EnumConstantDeclaration> entries, List<BodyDeclaration> members) {
        super(line, column, name, modifiers, members);
        this.annotations = annotations;
        this.implementsList = implementsList;
        this.entries = entries;
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
