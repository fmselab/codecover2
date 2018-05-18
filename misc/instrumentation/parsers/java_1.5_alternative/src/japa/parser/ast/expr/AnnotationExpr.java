/*
 * Created on 21/11/2006
 */
package japa.parser.ast.expr;

/**
 * @author Julio Vilmar Gesser
 */
public abstract class AnnotationExpr extends Expression {

    public AnnotationExpr(int line, int column) {
        super(line, column);
    }

}
