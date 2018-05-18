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
 * @version 1.0 ($Id: CDLS0006.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDLS0006 extends junit.framework.TestCase {
    /**
     * CDLS0006: Save to a XML file, target location inaccessible
     */
    public void testCDLS0006() throws Exception {
        String sourceFile = "testfile.xml"; // this well be used as "f"
        
        //1.
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);
        TestSessionContainer tsc1 = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), logger, builder, sourceFile);
        
        //2.
        final File testDir = new File("test");
        testDir.mkdir();

        // create an inaccessible directory.
        // Note: This won't work as root. Hopefully no one will run this test
        // as root.
        final File gDir = new File("test" + File.separator + "g_dir");
        gDir.delete();
        assertFalse(gDir.exists());
        gDir.mkdir();
        assertTrue(gDir.exists());
        String[] cmd;
        if (System.getProperty("os.name").toLowerCase().contains("windows")) {
            cmd = new String[] {"attrib", "+r", gDir.getPath()};
        } else {
            cmd = new String[] {"chmod", "000", gDir.getPath()};
        }
        Process process = Runtime.getRuntime().exec(cmd);
        assertEquals(0, process.waitFor());
        final File g = new File("test" + File.separator + "gDir" + File.separator + "g");
        
        try {
            tsc1.save(g); // save(File file)
            fail();
        } catch (FileSaveException e) {
        }
        try {
            tsc1.save(g.getPath()); // save(String filename)
            fail();
        } catch (FileSaveException e) {
        }
        // The following call would try to save to the original file, so
        // we can't test this
        /*
        try {
            tsc1.save(); // save()
            fail();
        } catch (FileSaveException e) {
        }
        */
        
        gDir.delete();
    }
}
