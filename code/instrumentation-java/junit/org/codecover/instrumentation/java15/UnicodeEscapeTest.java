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

package org.codecover.instrumentation.java15;

import static org.codecover.UtilsForTestingJava.TEST_SOURCE;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.model.utils.file.FileTool;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: UnicodeEscapeTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class UnicodeEscapeTest extends TestCase {

    public static final Charset CODE_EXAMPLE_ESCAPED_CHARSET = Charset.forName("Cp285");

    public static void main(String[] args) throws Exception {
        makeCodeExampleUnicodeEscaped();
        // System.out.print(toUnicodeEscape(">"));
        // System.out.print(fromUnicodeEscape("\\u0020\\u0043\\u006f\\u0070\\u0079\\u0072\\u0069\\u0067\\u0068\\u0074\\u0020\\u0028"));
    }

    private static void makeCodeExampleUnicodeEscaped() throws Exception {
        String srcPath = TEST_SOURCE + "test3/CodeExample.java";
        File source = new File(srcPath);
        String targetPath = TEST_SOURCE + "test8/CodeExample.java";
        File target = new File(targetPath);

        String contentOfCodeExample = FileTool.getContentFromFile(source);
        int length = contentOfCodeExample.length();
        StringBuilder contentWithoutEscapes = new StringBuilder(contentOfCodeExample.length());

        // we change the package
        contentWithoutEscapes.append("package org.codecover.instrumentation.java15.test.test8;");

        // we skip the package and 
        // have to transform any Unicode escaping \\uxxxx to the real character
        for (int i = contentOfCodeExample.indexOf('\n'); i < length; i++) {
            char c = contentOfCodeExample.charAt(i);
            if (i < (length - 5) &&
                c == '\\' &&
                contentOfCodeExample.charAt(i + 1) == 'u') {
               String hexNumber = contentOfCodeExample.substring(i + 2, i + 6);
               int iFromHex = Integer.parseInt(hexNumber, 16);
               char[] cUnescaped = Character.toChars(iFromHex);
               contentWithoutEscapes.append(cUnescaped);

               i += 5;
               continue;
            }

            contentWithoutEscapes.append(c);
        }

        StringBuilder contentFullyEscaped = toUnicodeEscape(contentWithoutEscapes);

        try {
            FileOutputStream fileOutputStream = new FileOutputStream(target);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(fileOutputStream, CODE_EXAMPLE_ESCAPED_CHARSET);
            outputStreamWriter.write("// created with: " + UnicodeEscapeTest.class.getName() + "\n");
            outputStreamWriter.write(contentFullyEscaped.toString());
            outputStreamWriter.flush();
            outputStreamWriter.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public static StringBuilder toUnicodeEscape(CharSequence toEscape) {
        StringBuilder buffer = new StringBuilder(toEscape.length() * 6);

        for (int i = 0; i < toEscape.length(); i++) {
            buffer.append((String.format("\\u%04x",
                    new Integer(Character.codePointAt(toEscape, i)))));
        }

        return buffer;
    }

    public static StringBuilder fromUnicodeEscape(CharSequence fromEscape) {
        StringBuilder buffer = new StringBuilder(fromEscape.length());

        for (int i = 0; i < fromEscape.length() / 6; i++) {
            int thisPos = i * 6;
            CharSequence hex = fromEscape.subSequence(thisPos + 2, thisPos + 6);
            int parsedHex = Integer.parseInt(hex.toString(), 16);
            buffer.append((char)parsedHex);
        }

        return buffer;
    }

    public void testUnicodeEquals() {
        String s1 = "\u005b\u00a9\u005D\u0020\u043F\u0440\u0438\u0432\u0435\u0442";
        String s2 = "[©] привет";

        Assert.assertEquals(s1, s2);
    }

    public void testCodeExampleEscapedParses() {
        String targetPath = TEST_SOURCE + "test8/CodeExample.java";
        File target = new File(targetPath);

        UtilsForTestingJava.isCompileableJava(target, CODE_EXAMPLE_ESCAPED_CHARSET);
    }
}
