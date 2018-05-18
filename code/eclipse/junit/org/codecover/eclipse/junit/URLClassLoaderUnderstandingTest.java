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
package org.codecover.eclipse.junit;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: URLClassLoaderUnderstandingTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class URLClassLoaderUnderstandingTest extends TestCase {

    public void testInClassPath() throws Exception {
        File thisDir = new File("/").getCanonicalFile();
        Assert.assertTrue(thisDir.exists());

        URL asURL = thisDir.toURL();
        URL[] urlArray = new URL[]{asURL};
        URLClassLoader urlClassLoader1 = new URLClassLoader(urlArray, this.getClass().getClassLoader());
        urlClassLoader1.loadClass("org.codecover.eclipse.junit.URLClassLoaderUnderstandingTest");

        URLClassLoader urlClassLoader2 = new URLClassLoader(urlArray, null);
        try {
            urlClassLoader2.loadClass("org.codecover.eclipse.junit.URLClassLoaderUnderstandingTest");
            Assert.fail("ClassNotFoundException expected");
        } catch (ClassNotFoundException e) {
            // expected
        }
    }
    
    public void testFileAbsolut() throws Exception {
        File file1 = new File("/").getCanonicalFile();
        Assert.assertTrue(file1.isAbsolute());

        File file2 = new File("bin/../relativeFile.txt");
        Assert.assertFalse(file2.isAbsolute());
        
        file2 = new File(file1.getAbsolutePath(), "bin/../relativeFile.txt").getCanonicalFile();
        Assert.assertTrue(file2.isAbsolute());

        File file3 = new File(file1.getCanonicalPath() + "/bin/../relativeFile.txt").getCanonicalFile();
        Assert.assertEquals(file2, file3);
    }
}
