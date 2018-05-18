/*
 * Created on 05/10/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.TypeParameter;
import japa.parser.ast.expr.AnnotationExpr;
import japa.parser.ast.type.ClassOrInterfaceType;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class ClassOrInterfaceDeclaration extends TypeDeclaration {

    public final List<AnnotationExpr> annotations;

    public final boolean isInterface;

    public final List<TypeParameter> typeParameters;

    public final List<ClassOrInterfaceType> extendsList;

    public final List<ClassOrInterfaceType> implementsList;

    public ClassOrInterfaceDeclaration(int line, int column, int modifiers, List<AnnotationExpr> annotations, boolean isInterface, String name, List<TypeParameter> typeParameters, List<ClassOrInterfaceType> extendsList, List<ClassOrInterfaceType> implementsList, List<BodyDeclaration> members) {
        super(line, column, name, modifiers, members);
        this.annotations = annotations;
        this.isInterface = isInterface;
        this.typeParameters = typeParameters;
        this.extendsList = extendsList;
        this.implementsList = implementsList;
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
