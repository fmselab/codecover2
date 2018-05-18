/*
 * Created on 04/11/2006
 */
package japa.parser.ast.stmt;

import japa.parser.ast.body.TypeDeclaration;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class TypeDeclarationStmt extends Statement {

    public final TypeDeclaration typeDecl;

    public TypeDeclarationStmt(int line, int column, TypeDeclaration typeDecl) {
        super(line, column);
        this.typeDecl = typeDecl;
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
