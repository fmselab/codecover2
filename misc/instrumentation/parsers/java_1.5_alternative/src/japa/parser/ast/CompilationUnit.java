/*
 * Created on 05/10/2006
 */
package japa.parser.ast;

import japa.parser.ast.body.TypeDeclaration;
import japa.parser.ast.expr.NameExpr;
import japa.parser.ast.visitor.GenericVisitor;
import japa.parser.ast.visitor.VoidVisitor;

import java.util.List;

/**
 * @author Julio Vilmar Gesser
 */
public final class CompilationUnit extends Node {

    public final NameExpr pakage;

    public final List<ImportDeclaration> imports;

    public final List<TypeDeclaration> types;

    public CompilationUnit(int line, int column, NameExpr pakage, List<ImportDeclaration> imports, List<TypeDeclaration> types) {
        super(line, column);
        this.pakage = pakage;
        this.imports = imports;
        this.types = types;
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
