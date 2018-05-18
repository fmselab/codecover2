/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation.java15.parser;

import java.io.IOException;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.java15.UnicodeEscapeTest;

/**
 * We want to test all cases of {@link JavaCharStream}.adjustBuffersize.<br>
 * There is:
 * <ul>
 * <li>1: this.available == this.bufsize - 1</li>
 * <ul>
 * <li>1a this.tokenBegin > BUFFER_SIZE</li>
 * <li>1b</li>
 * </ul>
 * <li>2 this.available > this.tokenBegin</li>
 * <li>3 (this.tokenBegin - this.available) < BUFFER_SIZE</li>
 * <li>4</li>
 * </ul>
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JavaCharStreamTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JavaCharStreamTest extends TestCase {
    public void test1st3rd() throws Exception {
        JavaCharStream jCS = createCharStream(16104);
        jCS.beginToken();
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(1, jCS.getEndOffset());

        for (int i = 1; i <= 3000; i++) {
            jCS.readChar();
            Assert.assertEquals(i + 1, jCS.getEndOffset());
        }

        jCS.backup(50);
        Assert.assertEquals(2951, jCS.getEndOffset());

        jCS.beginToken();
        Assert.assertEquals(2951, jCS.getStartOffset());
        Assert.assertEquals(2952, jCS.getEndOffset());

        for (int i = 1; i <= 5000; i++) {
            jCS.readChar();
            Assert.assertEquals(2951, jCS.getStartOffset());
            Assert.assertEquals(2952 + i, jCS.getEndOffset());
        }

        jCS.beginToken();
        Assert.assertEquals(7952, jCS.getStartOffset());
        Assert.assertEquals(7953, jCS.getEndOffset());

        for (int i = 1; i <= 200; i++) {
            jCS.readChar();
            Assert.assertEquals(7952, jCS.getStartOffset());
            Assert.assertEquals(7953 + i, jCS.getEndOffset());
        }

        jCS.backup(50);
        Assert.assertEquals(8103, jCS.getEndOffset());

        jCS.beginToken();
        Assert.assertEquals(8103, jCS.getStartOffset());
        Assert.assertEquals(8104, jCS.getEndOffset());

        // no wrap around
        for (int i = 1; i <= 8000; i++) {
            jCS.readChar();
            Assert.assertEquals(8103, jCS.getStartOffset());
            Assert.assertEquals(8104 + i, jCS.getEndOffset());
        }
    }
    
    public void test2nd() throws Exception {
        JavaCharStream jCS = createCharStream(9000);
        jCS.beginToken();
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(1, jCS.getEndOffset());

        for (int i = 2; i <= 3000; i++) {
            jCS.readChar();
            Assert.assertEquals(0, jCS.getStartOffset());
            Assert.assertEquals(i, jCS.getEndOffset());
        }

        jCS.readChar();
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(3001, jCS.getEndOffset());

        jCS.backup(1);
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(3000, jCS.getEndOffset());

        jCS.beginToken();
        Assert.assertEquals(3000, jCS.getStartOffset());
        Assert.assertEquals(3001, jCS.getEndOffset());

        for (int i = 2; i <= 3000; i++) {
            // 1a)
            jCS.readChar();
            Assert.assertEquals(3000, jCS.getStartOffset());
            Assert.assertEquals(3000 + i, jCS.getEndOffset());
        }

        jCS.readChar();
        Assert.assertEquals(3000, jCS.getStartOffset());
        Assert.assertEquals(6001, jCS.getEndOffset());

        jCS.backup(1);
        Assert.assertEquals(3000, jCS.getStartOffset());
        Assert.assertEquals(6000, jCS.getEndOffset());

        jCS.beginToken();
        Assert.assertEquals(6000, jCS.getStartOffset());
        Assert.assertEquals(6001, jCS.getEndOffset());

        for (int i = 2; i <= 3000; i++) {
            // 2), 1b)
            jCS.readChar();
            Assert.assertEquals(6000, jCS.getStartOffset());
            Assert.assertEquals(6000 + i, jCS.getEndOffset());
        }
    }

    public void test4th() throws Exception {
        JavaCharStream jCS = createCharStream(20000);
        jCS.beginToken();
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(1, jCS.getEndOffset());

        for (int i = 2; i <= 7000; i++) {
            // 1b), 1b)
            jCS.readChar();
            Assert.assertEquals(0, jCS.getStartOffset());
            Assert.assertEquals(i, jCS.getEndOffset());
        }

        // tokenPos = 0
        jCS.beginToken();
        Assert.assertEquals(7000, jCS.getStartOffset());
        Assert.assertEquals(7001, jCS.getEndOffset());

        for (int i = 2; i <= 4000; i++) {
            jCS.readChar();
            Assert.assertEquals(7000, jCS.getStartOffset());
            Assert.assertEquals(i + 7000, jCS.getEndOffset());
        }

        jCS.readChar();
        Assert.assertEquals(7000, jCS.getStartOffset());
        Assert.assertEquals(11001, jCS.getEndOffset());

        jCS.backup(1);
        Assert.assertEquals(7000, jCS.getStartOffset());
        Assert.assertEquals(11000, jCS.getEndOffset());

        // tokenPos = 4000
        jCS.beginToken();
        Assert.assertEquals(11000, jCS.getStartOffset());
        Assert.assertEquals(11001, jCS.getEndOffset());

        for (int i = 2; i <= 5000; i++) {
            // 1a)
            jCS.readChar();
            Assert.assertEquals(11000, jCS.getStartOffset());
            Assert.assertEquals(i + 11000, jCS.getEndOffset());
        }

        jCS.backup(1000);
        Assert.assertEquals(11000, jCS.getStartOffset());
        Assert.assertEquals(15000, jCS.getEndOffset());

        // tokenPos = 8000
        jCS.beginToken();
        Assert.assertEquals(15000, jCS.getStartOffset());
        Assert.assertEquals(15001, jCS.getEndOffset());

        for (int i = 2; i <= 5000; i++) {
            // 4)
            jCS.readChar();
            Assert.assertEquals(15000, jCS.getStartOffset());
            Assert.assertEquals(i + 15000, jCS.getEndOffset());
        }
    }

    public void testGetParsedImage1() throws Exception {
        JavaCharStream jCS = createCharStream(4097);
        StringBuilder builder = new StringBuilder(4097);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        String builderToString = builder.toString();
        Assert.assertEquals(builderToString, jCS.getParsedImage());
        Assert.assertEquals(builderToString, jCS.getSourceFileImage());
        
        for (int number = 2; number <= 4097; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            builderToString = builder.toString();
            Assert.assertEquals(builderToString, jCS.getParsedImage());
            Assert.assertEquals(builderToString, jCS.getSourceFileImage());
        }
    }

    public void testGetParsedImage2() throws Exception {
        JavaCharStream jCS = createCharStream(5000);
        StringBuilder builder = new StringBuilder(5000);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        String builderToString = builder.toString();
        Assert.assertEquals(builderToString, jCS.getParsedImage());
        Assert.assertEquals(builderToString, jCS.getSourceFileImage());

        for (int number = 2; number <= 3001; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            builderToString = builder.toString();
            Assert.assertEquals(builderToString, jCS.getParsedImage());
            Assert.assertEquals(builderToString, jCS.getSourceFileImage());
            Assert.assertEquals(0, jCS.getStartOffset());
            Assert.assertEquals(number, jCS.getEndOffset());
        }

        jCS.backup(1);
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(3000, jCS.getEndOffset());

        // tokenPos = 3000
        Assert.assertEquals('1', jCS.beginToken());
        Assert.assertEquals(3000, jCS.getStartOffset());
        Assert.assertEquals(3001, jCS.getEndOffset());
        builder.setLength(1);
        builderToString = builder.toString();
        Assert.assertEquals(builderToString, jCS.getParsedImage());
        Assert.assertEquals(builderToString, jCS.getSourceFileImage());

        for (int number = 3002; number <= 5000; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            // the else case of getParsedImage
            builderToString = builder.toString();
            Assert.assertEquals(builderToString, jCS.getParsedImage());
            Assert.assertEquals(builderToString, jCS.getSourceFileImage());
            Assert.assertEquals(3000, jCS.getStartOffset());
            Assert.assertEquals(number, jCS.getEndOffset());
        }
    }

    public void testGetSuffix1() throws Exception {
        JavaCharStream jCS = createCharStream(5000);
        StringBuilder builder = new StringBuilder(100);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(1)));

        for (int number = 2; number <= 100; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(number)));
            Assert.assertEquals(100, builder.capacity());
        }

        for (int number = 101; number <= 5000; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.deleteCharAt(0);
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(100)));
            Assert.assertEquals(100, builder.capacity());
        }
    }
    
    public void testGetSuffix2() throws Exception {
        JavaCharStream jCS = createCharStream(5000);
        StringBuilder builder = new StringBuilder(100);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(1)));
        
        for (int number = 2; number <= 100; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(number)));
            Assert.assertEquals(100, builder.capacity());
        }

        for (int number = 101; number <= 3001; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.deleteCharAt(0);
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(100)));
            Assert.assertEquals(100, builder.capacity());
        }

        jCS.backup(1);
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(3000, jCS.getEndOffset());

        // tokenPos = 3000
        Assert.assertEquals('1', jCS.beginToken());
        Assert.assertEquals(3000, jCS.getStartOffset());
        Assert.assertEquals(3001, jCS.getEndOffset());
        builder.setLength(0);
        builder.append('1');
        Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(1)));

        for (int number = 3002; number <= 3100; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(number - 3000)));
            Assert.assertEquals(100, builder.capacity());
            Assert.assertEquals(3000, jCS.getStartOffset());
            Assert.assertEquals(number, jCS.getEndOffset());
        }

        for (int number = 3101; number <= 5000; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.deleteCharAt(0);
            builder.append(thisChar);
            // the else branch of getSuffix
            Assert.assertEquals(builder.toString(),  new String(jCS.getSuffix(100)));
            Assert.assertEquals(100, builder.capacity());
            Assert.assertEquals(3000, jCS.getStartOffset());
            Assert.assertEquals(number, jCS.getEndOffset());
        }
    }

    public void testEscapedUnicodes1() throws Exception {
        String escaped = UnicodeEscapeTest.toUnicodeEscape(createStringBuilder(5000)).toString();
        JavaCharStream jCS = new JavaCharStream(escaped);
        StringBuilder builder = new StringBuilder(100);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(1)));
        Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(6, jCS.getEndOffset());
        Assert.assertEquals(escaped.substring(0, 1 * 6), new String(jCS.getSourceFileImage()));

        for (int number = 2; number <= 100; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(number)));
            Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
            Assert.assertEquals(escaped.substring(0, number * 6), new String(jCS.getSourceFileImage()));
            Assert.assertEquals(100, builder.capacity());
        }
    
        for (int number = 101; number <= 5000; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
            Assert.assertEquals(escaped.substring(0, number * 6), new String(jCS.getSourceFileImage()));
        }
    }

    public void testEscapedUnicodes2() throws Exception {
        String escaped = UnicodeEscapeTest.toUnicodeEscape(createStringBuilder(5000)).toString();
        JavaCharStream jCS = new JavaCharStream(escaped);
        StringBuilder builder = new StringBuilder(100);
        Assert.assertEquals('1', jCS.beginToken());
        builder.append('1');
        Assert.assertEquals(builder.toString(), new String(jCS.getSuffix(1)));
        Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(6, jCS.getEndOffset());
        Assert.assertEquals(escaped.substring(0, 1 * 6), new String(jCS.getSourceFileImage()));

        for (int number = 2; number <= 3001; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
            Assert.assertEquals(escaped.substring(0, number * 6), new String(jCS.getSourceFileImage()));
            Assert.assertEquals(0, jCS.getStartOffset());
            Assert.assertEquals(number * 6, jCS.getEndOffset());
        }

        jCS.backup(1);
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(18000, jCS.getEndOffset());

        // tokenPos = 3000
        Assert.assertEquals('1', jCS.beginToken());
        Assert.assertEquals(18000, jCS.getStartOffset());
        Assert.assertEquals(18006, jCS.getEndOffset());
        builder.setLength(0);
        builder.append('1');
        Assert.assertEquals(builder.toString(), jCS.getParsedImage());
        Assert.assertEquals(escaped.substring(18000, 18006), jCS.getSourceFileImage());

        for (int number = 3002; number <= 5000; number++) {
            char thisChar = Character.forDigit(number % 10, 10);
            Assert.assertEquals(thisChar, jCS.readChar());
            builder.append(thisChar);
            Assert.assertEquals(builder.toString(), new String(jCS.getParsedImage()));
            Assert.assertEquals(escaped.substring(18000, number * 6), new String(jCS.getSourceFileImage()));
            Assert.assertEquals(18000, jCS.getStartOffset());
            Assert.assertEquals(number * 6, jCS.getEndOffset());
        }
    }

    public void testUnescpaped() throws Exception {
        String source = "\\\\" +
                        "o" +
                        "\\n" +
                        "\\u0020" +
                        "\\uu0020" +
                        "\\uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu0020" +
                        "\\\\\\u0020" +
                        "\\\\u0020";

        JavaCharStream jCS = new JavaCharStream(source);

        Assert.assertEquals('\\', jCS.beginToken());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals('o', jCS.readChar());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals('n', jCS.readChar());
        Assert.assertEquals(' ', jCS.readChar());
        Assert.assertEquals(' ', jCS.readChar());
        Assert.assertEquals(' ', jCS.readChar());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals(' ', jCS.readChar());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals('\\', jCS.readChar());
        Assert.assertEquals('u', jCS.readChar());
        Assert.assertEquals('0', jCS.readChar());
        Assert.assertEquals('0', jCS.readChar());
        Assert.assertEquals('2', jCS.readChar());
        Assert.assertEquals('0', jCS.readChar());

        Assert.assertEquals(source, jCS.getSourceFileImage());
    }

    public void testUnescpapedError() throws Exception {
        String source = "\\u0p20";

        JavaCharStream jCS = new JavaCharStream(source);
        try {
            jCS.readChar();
            Assert.fail("Error excpected");
        } catch (Error e) {
            Assert.assertEquals("Invalid escape character: \\u0p20", e.getMessage());
        }
    }
   
    public void testEnd1() throws Exception {
        String source = "AUDI";
        JavaCharStream jCS = new JavaCharStream(source);
        Assert.assertEquals('A', jCS.beginToken());
        Assert.assertEquals('U', jCS.readChar());
        Assert.assertEquals('D', jCS.readChar());
        Assert.assertEquals('I', jCS.readChar());
        try {
            jCS.readChar();
            Assert.fail("IOException expected");
        } catch (IOException e) {
            // expected
        }
        Assert.assertEquals(source, jCS.getParsedImage());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(4, jCS.getEndOffset());
    }

    public void testEnd2() throws Exception {
        String source = "AUDI";
        JavaCharStream jCS = new JavaCharStream(source);
        Assert.assertEquals('A', jCS.beginToken());
        Assert.assertEquals('U', jCS.readChar());
        Assert.assertEquals('D', jCS.readChar());
        Assert.assertEquals('I', jCS.readChar());
        Assert.assertEquals(source, jCS.getParsedImage());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(4, jCS.getEndOffset());
        try {
            jCS.beginToken();
            Assert.fail("IOException expected");
        } catch (IOException e) {
            // expected
        }
        Assert.assertEquals("", jCS.getSourceFileImage());
        Assert.assertEquals(4, jCS.getStartOffset());
        Assert.assertEquals(4, jCS.getEndOffset());
    }

    public void testLineColumn() throws Exception {
        String source = "ABCD\nEF\r\nGH";
        JavaCharStream jCS = new JavaCharStream(source);

        Assert.assertEquals('A', jCS.beginToken());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(1, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(1, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(1, jCS.getEndOffset());

        Assert.assertEquals('B', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(1, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(2, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(2, jCS.getEndOffset());

        Assert.assertEquals('C', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(1, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(3, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(3, jCS.getEndOffset());

        Assert.assertEquals('D', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(1, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(4, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(4, jCS.getEndOffset());

        Assert.assertEquals('\n', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(1, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(5, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(5, jCS.getEndOffset());

        Assert.assertEquals('E', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(2, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(1, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(6, jCS.getEndOffset());

        Assert.assertEquals('F', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(2, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(2, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(7, jCS.getEndOffset());

        Assert.assertEquals('\r', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(2, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(3, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(8, jCS.getEndOffset());

        Assert.assertEquals('\n', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(2, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(4, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(9, jCS.getEndOffset());

        Assert.assertEquals('G', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(3, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(1, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(10, jCS.getEndOffset());

        Assert.assertEquals('H', jCS.readChar());
        Assert.assertEquals(1, jCS.getBeginLine());
        Assert.assertEquals(3, jCS.getEndLine());
        Assert.assertEquals(1, jCS.getBeginColumn());
        Assert.assertEquals(2, jCS.getEndColumn());
        Assert.assertEquals(0, jCS.getStartOffset());
        Assert.assertEquals(11, jCS.getEndOffset());
    }

    private static final int MAXSIZE = 1024 * 64;

    private JavaCharStream createCharStream() {
        return createCharStream(MAXSIZE);
    }

    private JavaCharStream createCharStream(int size) {
        JavaCharStream jCS = new JavaCharStream(createStringBuilder(size).toString());
        return jCS;
    }

    private StringBuilder createStringBuilder(int size) {
        StringBuilder builder = new StringBuilder(size);
        for (int number = 1; number <= size; number++) {
            builder.append(number % 10);
        }
        
        return builder;
    }
}
