package org.codecover.instrumentation.c.adapter;

import org.anarres.cpp.*;
import org.codecover.instrumentation.c.InstrumenterDescriptor;
import org.codecover.instrumentation.c.parser.CParserConstants;
import org.codecover.instrumentation.c.parser.TokenManager;

import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.util.List;

public class TokenAdapter extends PreprocessorListener implements TokenManager {
    CCPreprocessor pp;
    boolean debug;

    public TokenAdapter(File source, List<String> includeDirs, List<String> defines, boolean debug) throws IOException, LexerException {
        pp = new CCPreprocessor(source);
        pp.setQuoteIncludePath(includeDirs);
        pp.setSystemIncludePath(includeDirs);
        for(String d : defines) {
            int i = d.indexOf('=');
            if(i == -1) {
                pp.addMacro(d);
            } else {
                pp.addMacro(d.substring(0,i), d.substring(i+1));
            }
        }

        pp.addMacro("__STDC__");

        this.debug = debug;

        pp.setListener(this);
        //pp.addFeature(Feature.VERBOSE);
        //pp.addFeature(Feature.DEBUG);
    }

    @Override
    public void handleSourceChange(Source source, String event) {
        if(debug && source != null)
            System.out.println(source.toString() + ": " +  event);
    }

    @Override
    public org.codecover.instrumentation.c.parser.Token getNextToken() {
        try {
            return convertToken();
        } catch (IOException e) {
            e.printStackTrace();
        } catch (LexerException e) {
            e.printStackTrace();
        }
        return null;
    }

    private org.codecover.instrumentation.c.parser.Token convertToken() throws IOException, LexerException {
        // Skip Whitespace and comments?
        int kind;
        Token t;
        do {
            t = pp.ccToken();
            //System.out.print(t.getText());
            kind = convertKind(t, t.getType());
        } while(kind == -1);
        if(kind == -2) {
            System.err.println("ERROR:");
            System.err.println(t.getText());
        }

        org.codecover.instrumentation.c.parser.Token tt =
                new org.codecover.instrumentation.c.parser.Token(kind,t.getText());
        // TODO: proper begin/end
        tt.beginColumn = t.getColumn() + 1;
        tt.endColumn = t.getText() != null ? t.getText().length() + tt.beginColumn - 1 : tt.beginColumn;
        tt.beginLine = tt.endLine = t.getLine();

        tt.beginOffset = t.offset;
        tt.endOffset = t.getText() != null ? t.getText().length() + tt.beginOffset : tt.beginOffset;

        return tt;
    }

    private static int convertKind(Token t, int type) {
        switch (type)
        {
            case '(':
                return CParserConstants.RBL;
            case ')':
                return CParserConstants.RBR;
            case '{':
                return CParserConstants.CBL;
            case '}':
                return CParserConstants.CBR;
            case '[':
                return CParserConstants.SBL;
            case ']':
                return CParserConstants.SBR;
            case '*':
                return CParserConstants.STAR;
            case '/':
                return CParserConstants.SLASH;
            case '%':
                return CParserConstants.MOD;
            case '+':
                return CParserConstants.PLUS;
            case '-':
                return CParserConstants.MINUS;
            case '!':
                return CParserConstants.EXCL;
            case '&':
                return CParserConstants.AMP;
            case '?':
                return CParserConstants.QEM;
            case '~':
                return CParserConstants.TILDE;
            case '^':
                return CParserConstants.CARE;
            case '|':
                return CParserConstants.PIPE;
            case ',':
                return CParserConstants.COMMA;
            case ';':
                return CParserConstants.SEMICOLON;
            case ':':
                return CParserConstants.COLON;
            case '=':
                return CParserConstants.ASSIGNMENT;
            case '.':
                return CParserConstants.DOT;
            case '<':
                return CParserConstants.LESS;
            case '>':
                return CParserConstants.GREATER;
            case Token.AND_EQ:
                return CParserConstants.AND_EQ;
            case Token.ARROW:
                return CParserConstants.ARROW;
            case Token.CHARACTER:
                return CParserConstants.CHARACTER_LITERAL;
            case Token.CCOMMENT:
                return -1;
            case Token.CPPCOMMENT:
                return -1;
            case Token.DEC:
                return CParserConstants.DEC;
            case Token.DIV_EQ:
                return CParserConstants.DIV_EQ;
            case Token.ELLIPSIS:
                return CParserConstants.ELLIPSIS;
            case Token.EOF:
                return CParserConstants.EOF;
            case Token.EQ:
                return CParserConstants.EQ;
            case Token.GE:
                return CParserConstants.GE;
            case Token.HASH:
                // TODO
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.HEADER:
                // TODO
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.IDENTIFIER:
                return convertIdentifier(t.getText());
            case Token.INC:
                return CParserConstants.INC;
            case Token.LAND:
                return CParserConstants.LAND;
            case Token.LAND_EQ:
                // TODO invalid
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.LE:
                return CParserConstants.LE;
            case Token.LITERAL:
                // TODO
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.LOR:
                return CParserConstants.LOR;
            case Token.LOR_EQ:
                return -2;
            case Token.LSH:
                return CParserConstants.LSH;
            case Token.LSH_EQ:
                return CParserConstants.LSH_EQ;
            case Token.MOD_EQ:
                return CParserConstants.MOD_EQ;
            case Token.MULT_EQ:
                return CParserConstants.MULT_EQ;
            case Token.NE:
                return CParserConstants.NE;
            case Token.NL:
                return -1;
            case Token.NUMBER:
                return CParserConstants.NUMBER;
            case Token.OR_EQ:
                return CParserConstants.OR_EQ;
            case Token.PASTE:
                // TODO invalid;
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.PLUS_EQ:
                return CParserConstants.PLUS_EQ;
            case Token.RANGE:
                // TODO invalid
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.RSH:
                return CParserConstants.RSH;
            case Token.RSH_EQ:
                return CParserConstants.RSH_EQ;
            case Token.STRING:
                return CParserConstants.STRING_LITERAL;
            case Token.SUB_EQ:
                return CParserConstants.SUB_EQ;
            case Token.WHITESPACE:
                return -1;
            case Token.XOR_EQ:
                return CParserConstants.XOR_EQ;
            case Token.M_ARG:
            case Token.M_PASTE:
            case Token.M_STRING:
            case Token.P_LINE:
                // TODO invalid
                System.out.println("TODO: convert id: " + type);
                return -2;
            case Token.INVALID:
                // TODO
                System.out.println("TODO: convert id: " + type);
                return -2;
            default:
                return type;
        }
    }

    private static int convertIdentifier(String image) {
        if("alignof".equals(image)) { return CParserConstants.ALIGNOF; }
        if("auto".equals(image)) { return CParserConstants.AUTO; }
        if("break".equals(image)) { return CParserConstants.BREAK; }
        if("case".equals(image)) { return CParserConstants.CASE; }
        if("char".equals(image)) { return CParserConstants.CHAR; }
        if("const".equals(image)) { return CParserConstants.CONST; }
        if("__const".equals(image)) { return CParserConstants.CONST; } // GCC specific
        if("continue".equals(image)) { return CParserConstants.CONTINUE; }
        if("default".equals(image)) { return CParserConstants.DFAULT; }
        if("do".equals(image)) { return CParserConstants.DO; }
        if("double".equals(image)) { return CParserConstants.DOUBLE; }
        if("else".equals(image)) { return CParserConstants.ELSE; }
        if("enum".equals(image)) { return CParserConstants.ENUM; }
        if("extern".equals(image)) { return CParserConstants.EXTERN; }
        if("float".equals(image)) { return CParserConstants.FLOAT; }
        if("for".equals(image)) { return CParserConstants.FOR; }
        if("goto".equals(image)) { return CParserConstants.GOTO; }
        if("if".equals(image)) { return CParserConstants.IF; }
        if("inline".equals(image)) { return CParserConstants.INLINE; }
        if("int".equals(image)) { return CParserConstants.INT; }
        if("long".equals(image)) { return CParserConstants.LONG; }
        if("register".equals(image)) { return CParserConstants.REGISTER; }
        if("restrict".equals(image)) { return CParserConstants.RESTRICT; }
        if("__restrict".equals(image)) { return CParserConstants.RESTRICT; } // GCC specific
        if("return".equals(image)) { return CParserConstants.RETURN; }
        if("short".equals(image)) { return CParserConstants.SHORT; }
        if("signed".equals(image)) { return CParserConstants.SIGNED; }
        if("sizeof".equals(image)) { return CParserConstants.SIZEOF; }
        if("static".equals(image)) { return CParserConstants.STATIC; }
        if("struct".equals(image)) { return CParserConstants.STRUCT; }
        if("switch".equals(image)) { return CParserConstants.SWITCH; }
        if("typedef".equals(image)) { return CParserConstants.TYPEDEF; }
        if("union".equals(image)) { return CParserConstants.UNION; }
        if("unsigned".equals(image)) { return CParserConstants.UNSIGNED; }
        if("void".equals(image)) { return CParserConstants.VOID; }
        if("volatile".equals(image)) { return CParserConstants.VOLATILE; }
        if("while".equals(image)) { return CParserConstants.WHILE; }
        if("_Alignas".equals(image)) { return CParserConstants.ALIGNAS; }
        if("_Atomic".equals(image)) { return CParserConstants.ATOMIC; }
        if("_Bool".equals(image)) { return CParserConstants.BOOL; }
        if("_Complex".equals(image)) { return CParserConstants.COMPLEX; }
        if("_Generic".equals(image)) { return CParserConstants.GENERIC; }
        if("_Imaginary".equals(image)) { return CParserConstants.IMAGINARY; }
        if("_Noreturn".equals(image)) { return CParserConstants.NORETURN; }
        if("_Static_assert".equals(image)) { return CParserConstants.STATICASSERT; }
        if("_Thread_local".equals(image)) { return CParserConstants.THREADLOCAL; }
        if("__attribute__".equals(image)) { return CParserConstants.ATTRIBUTE; }
        if("__extension__".equals(image)) { return CParserConstants.EXTENSION; }
        if("__nonnull".equals(image)) { return CParserConstants.NONNULL; }
        if("__asm__".equals(image)) { return CParserConstants.ASM; }
        else { return CParserConstants.IDENTIFIER;
        }
    }
}
