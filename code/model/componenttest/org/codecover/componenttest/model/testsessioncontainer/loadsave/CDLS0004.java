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

package org.codecover.componenttest.model.testsessioncontainer.loadsave;

import java.util.*;
import java.io.*;

import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.exceptions.*;
import org.codecover.model.utils.*;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CDLS0004.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0004 extends junit.framework.TestCase {

    private static void copyFile(File in, File out) throws Exception {
        final FileInputStream input = new FileInputStream(in);
        try {
            final FileOutputStream output = new FileOutputStream(out);
            try {
                final byte[] buf = new byte[4096];
                int readBytes = 0;
                while((readBytes = input.read(buf)) != -1) {
                    output.write(buf, 0, readBytes);
                }
            } finally {
                output.close();
            }
        } finally {
            input.close();
        }
    }

    private static void check(File file1, File file2) throws Exception {
        // We check for syntactical rather than for semantical equivalence
        // This might give us false positives, but can be done automatically
        
        final FileInputStream input1 = new FileInputStream(file1);
        try {
            final FileInputStream input2 = new FileInputStream(file2);
            try {
                int result1 = 0;
                int result2 = 0;
                while (result1 != -1) {
                    // Byte-by-byte check, slow, but easy
                    result1 = input1.read();
                    result2 = input2.read();
                    if (result1 != result2) {
                        fail(file1 + " and " + file2 + " differ. We check for syntactical rather than for semantical equivalence. This might give us false positives, but can be done automatically.");
                    }
                }
            } finally {
                input2.close();
            }
        } finally {
            input1.close();
        }
    }

    /**
     * CDLS0004: Save to a XML file
     *
     * NOTE:
     * {@code .} must reside on a filesystem that is different from the 
     * filesystem where the system temporary directory (determined by 
     * java.io.tmpdir system property) is located.
     */
    public void testCDLS0004() throws Exception {
        // Make preconditions true
        String sourceFile = "testfile.xml"; // this well be used as "f"
        
        final File testDir = new File("test");
        testDir.mkdir();
        
        final File f = new File("test" + File.separator + "f");
        f.delete();
        assertFalse(f.exists());
        copyFile(new File(sourceFile), f);
        assertTrue(f.exists());
        
        final File g = new File("test" + File.separator + "g");
        g.delete();
        assertFalse(g.exists());
        
        //1.
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, f);
        
        //2.
        tsc1.save(g); // save(File file)
        check(f, g);
        tsc1.save(g.getPath()); // save(String filename)
        check(f, g);
        tsc1.save(); // save()
        check(f, g);
    }
}
