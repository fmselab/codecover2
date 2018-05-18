/*
 * Created on 10/10/2006
 */
package japa.parser.ast.expr;

import japa.parser.ast.Node;

/**
 * @author Julio Vilmar Gesser
 */
public abstract class Expression extends Node {

    public Expression(int line, int column) {
        super(line, column);
    }

}
