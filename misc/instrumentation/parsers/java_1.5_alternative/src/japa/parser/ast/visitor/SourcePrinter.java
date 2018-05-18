/*
 * Created on 08/10/2006
 */
package japa.parser.ast.visitor;

/**
 * @author Julio Vilmar Gesser
 */
public final class SourcePrinter {

    private int level = 0;

    private boolean indented = false;

    private final StringBuilder buf = new StringBuilder();

    public void indent() {
        level++;
    }

    public void unindent() {
        level--;
    }

    private void makeIndent() {
        for (int i = 0; i < level; i++) {
            buf.append("    ");
        }
    }

    public void print(String arg) {
        if (!indented) {
            makeIndent();
            indented = true;
        }
        buf.append(arg);
    }

    public void printLn(String arg) {
        print(arg);
        printLn();
    }

    public void printLn() {
        buf.append("\n");
        indented = false;
    }

    public String getSource() {
        return buf.toString();
    }

    @Override
    public String toString() {
        return getSource();
    }
}