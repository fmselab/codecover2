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

package org.codecover.model.utils.file;

import static org.codecover.model.utils.file.FileTool.getExtension;

import java.io.File;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: FileToolTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class FileToolTest extends TestCase {

    public void testGetExtension() {
        String name;
        File file;

        name = "hello.java";
        file = new File(name);
        Assert.assertEquals("java", getExtension(file));

        name = "hello.java.in";
        file = new File(name);
        Assert.assertEquals("in", getExtension(file));

        name = "hello";
        file = new File(name);
        Assert.assertEquals("", getExtension(file));

        name = "";
        file = new File(name);
        Assert.assertEquals("", getExtension(file));

        name = "hello.java";
        file = new File(name);
        Assert.assertEquals("java", getExtension(file));

        name = "hello.";
        file = new File(name);
        Assert.assertEquals("", getExtension(file));

        name = "hello.java.JAVA";
        file = new File(name);
        Assert.assertEquals("java", getExtension(file));
    }
}
