/*
 * Created on 05/10/2006
 */
package japa.parser.ast.body;

import japa.parser.ast.Node;

/**
 * @author Julio Vilmar Gesser
 */
public abstract class BodyDeclaration extends Node {

    public BodyDeclaration(int line, int column) {
        super(line, column);
    }

}
