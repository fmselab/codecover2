/*
 * Created on 22/11/2006
 */
package japa.parser.ast.test;

import japa.parser.JavaParser;
import japa.parser.ast.CompilationUnit;

import java.io.StringBufferInputStream;

import junit.framework.TestCase;

/**
 * @author Julio Vilmar Gesser
 */
public class TestAstToString extends TestCase {

    private final String source = //
    "package japa.parser.javacc;\n" + //
            "\n" + //
            "import japa.parser.JavaParser;\n" + //
            "import japa.parser.ParseException;\n" + //
            "import japa.parser.ast.CompilationUnit;\n" + //
            "import java.io.File;\n" + //
            "import java.io.IOException;\n" + //
            "import java.io.Serializable;\n" + //
            "import java.util.ArrayList;\n" + //
            "import java.util.Comparator;\n" + //
            "import java.util.List;\n" + //
            "import java.util.Map;\n" + //
            "\n" + //
            "@Deprecated\n" + //
            "public class Teste<T extends List<int[]>, X> extends Object implements Serializable {\n" + //
            "\n" + //
            "    int[] arr = new int[10];\n" + //
            "\n" + //
            "    ;\n" + //
            "\n" + //
            "    ;\n" + //
            "\n" + //
            "    @Deprecated\n" + //
            "    int[][][][] arr2 = new int[10][2][1][0];\n" + //
            "\n" + //
            "    char cc = 'a';\n" + //
            "\n" + //
            "    int[][] arr3 = { { 1, 2 }, { 3, 4 } };\n" + //
            "\n" + //
            "    static int[] arr4[] = {};\n" + //
            "\n" + //
            "    static {\n" + //
            "        arr4 = new int[][] { { 2 }, { 1 } };\n" + //
            "    }\n" + //
            "\n" + //
            "    public final enum Teste {\n" + //
            "\n" + //
            "        asc, def\n" + //
            "    }\n" + //
            "\n" + //
            "    public static enum Sexo {\n" + //
            "\n" + //
            "        m, f;\n" + //
            "\n" + //
            "        public static enum Sexo_ implements Serializable {\n" + //
            "\n" + //
            "        }\n" + //
            "\n" + //
            "        private Sexo() {\n" + //
            "        }\n" + //
            "    }\n" + //
            "\n" + //
            "    public static enum Enum {\n" + //
            "\n" + //
            "        m(1) {\n" + //
            "\n" + //
            "            void mm() {\n" + //
            "            }\n" + //
            "        }\n" + //
            "        , f(2) {\n" + //
            "\n" + //
            "            void mm() {\n" + //
            "            }\n" + //
            "        }\n" + //
            "        ;\n" + //
            "\n" + //
            "        int x;\n" + //
            "\n" + //
            "        private Enum(int x) {\n" + //
            "            this.x = x;\n" + //
            "        }\n" + //
            "\n" + //
            "        abstract void mm();\n" + //
            "    }\n" + //
            "\n" + //
            "    public <T> Teste(int x) {\n" + //
            "        this.arr[0] = x;\n" + //
            "        boolean b = true, y = false;\n" + //
            "        abstract class X {\n" + //
            "\n" + //
            "            public X() {\n" + //
            "            }\n" + //
            "\n" + //
            "            public void m() {\n" + //
            "            }\n" + //
            "        }\n" + //
            "        @Deprecated\n" + //
            "        final class Y extends X {\n" + //
            "\n" + //
            "            public Y() {\n" + //
            "                super();\n" + //
            "                Y.super.m();\n" + //
            "            }\n" + //
            "\n" + //
            "            public Y(int x) {\n" + //
            "                this();\n" + //
            "            }\n" + //
            "        }\n" + //
            "    }\n" + //
            "\n" + //
            "    private class QWE extends Teste<List<int[]>, String> {\n" + //
            "\n" + //
            "        @Deprecated\n" + //
            "        final int z = 0;\n" + //
            "\n" + //
            "        int i = 0;\n" + //
            "\n" + //
            "        public QWE(int... x) {\n" + //
            "            super(x[0]);\n" + //
            "            i = x[0];\n" + //
            "            assert true;\n" + //
            "            assert 1 == 1 : 2;\n" + //
            "            {\n" + //
            "                int iii = 3;\n" + //
            "                iii += 3;\n" + //
            "            }\n" + //
            "            label: {\n" + //
            "                int iii = 1;\n" + //
            "            }\n" + //
            "            ;\n" + //
            "            ;\n" + //
            "            switch(i) {\n" + //
            "            }\n" + //
            "            ll: switch(i) {\n" + //
            "                case 1:\n" + // 
            "                    System.out.println(1);\n" + //
            "                    break ll;\n" + //
            "                default:\n" + // 
            "                    {\n" + //
            "                        System.out.println(\"default\");\n" + //
            "                        break;\n" + //
            "                    }\n" + //
            "                case 2:\n" + //
            "                    System.out.println(1);\n" + //
            "                    i++;\n" + //
            "                    ++i;\n" + //
            "            }\n" + //
            "        }\n" + //
            "\n" + //
            "        private synchronized int doSomething()[] {\n" + //
            "            List<? extends Number> x = new ArrayList<Integer>();\n" + //
            "            return new int[] { 1 };\n" + //
            "        }\n" + //
            "    }\n" + //
            "\n" + //
            "    public static void main(String[] args) throws ParseException, IOException {\n" + //
            "        int x = 2;\n" + //
            "        CompilationUnit cu = parse(new File(\"src/japa/parser/javacc/Parser.java\"));\n" + //
            "        System.out.println(cu);\n" + //
            "        Teste teste = new Teste(2);\n" + //
            "        Teste.QWE qwe = teste.new QWE(1);\n" + //
            "        if (1 + 1 == 2) {\n" + //
            "            teste = null;\n" + //
            "            teste = new Teste(1);\n" + //
            "        } else {\n" + //
            "            x = 3;\n" + //
            "            teste = new Teste(1);\n" + //
            "            x = 2;\n" + //
            "        }\n" + //
            "        if (true) x = 1; else x = 3;\n" + //
            "        while (true) {\n" + //
            "            while (x == 3) continue;\n" + //
            "            break;\n" + //
            "        }\n" + //
            "        do {\n" + //
            "            x++;\n" + //
            "        } while (x < 100);\n" + //
            "        do x++; while (x < 100);\n" + //
            "        for (@Deprecated int i : arr4[0]) {\n" + //
            "            x--;\n" + //
            "        }\n" + //
            "        for (@Deprecated final int i = 0, j = 1; i < 10; x++) {\n" + //
            "            break;\n" + //
            "        }\n" + //
            "        int i, j;\n" + //
            "        for (i = 0, j = 1; i < 10 && j < 2; i++, j--) {\n" + //
            "            break;\n" + //
            "        }\n" + //
            "    }\n" + //
            "\n" + //
            "    @AnnotationTest(value = \"x\")\n" + //
            "    public static CompilationUnit parse(@Deprecated File file) throws ParseException, IOException {\n" + //
            "        synchronized (file) {\n" + //
            "            file = null;\n" + //
            "            file = new File(\"\");\n" + //
            "        }\n" + //
            "        try {\n" + //
            "            if (file == null) {\n" + //
            "                throw new NullPointerException(\"blah\");\n" + //
            "            }\n" + //
            "        } catch (final NullPointerException e) {\n" + //
            "            System.out.println(\"catch\");\n" + //
            "        } catch (RuntimeException e) {\n" + //
            "            System.out.println(\"catch\");\n" + //
            "        } finally {\n" + //
            "            System.out.println(\"finally\");\n" + //
            "        }\n" + //
            "        try {\n" + //
            "            if (file == null) {\n" + //
            "                throw new NullPointerException(\"blah\");\n" + //
            "            }\n" + //
            "        } finally {\n" + //
            "            System.out.println(\"finally\");\n" + //
            "        }\n" + //
            "        try {\n" + //
            "            if (file == null) {\n" + //
            "                throw new NullPointerException(\"blah\");\n" + //
            "            }\n" + //
            "        } catch (RuntimeException e) {\n" + //
            "            System.out.println(\"catch\");\n" + //
            "        }\n" + //
            "        return JavaParser.parse(file);\n" + //
            "    }\n" + //
            "\n" + //
            "    private <Y> void x(Map<? extends X, ? super T> x) {\n" + //
            "        @Deprecated Comparator c = new Comparator() {\n" + //
            "\n" + //
            "            public int compare(Object o1, Object o2) {\n" + //
            "                return 0;\n" + //
            "            }\n" + //
            "\n" + //
            "            @Override\n" + //
            "            public boolean equals(Object obj) {\n" + //
            "                return super.equals(obj);\n" + //
            "            }\n" + //
            "        };\n" + //
            "    }\n" + //
            "\n" + //
            "    public @interface AnnotationTest {\n" + //
            "\n" + //
            "        String value() default \"asd\";\n" + //
            "\n" + //
            "        @Deprecated\n" + //
            "        int[] valueI() default { 1, 2 };\n" + //
            "\n" + //
            "        AnnotationTest valueA1() default @AnnotationTest;\n" + //
            "\n" + //
            "        AnnotationTest valueA2() default @AnnotationTest(\"qwe\");\n" + //
            "\n" + //
            "        AnnotationTest valueA3() default @AnnotationTest(value = \"qwe\", valueI = { 1 });\n" + //
            "    }\n" + //
            "\n" + //
            "    ;\n" + //
            "}\n";

    public void testDumpVisitor() throws Exception {
        System.out.println(source);
        CompilationUnit cu = JavaParser.parse(new StringBufferInputStream(source));
        assertEquals(source, cu.toString());
    }

}