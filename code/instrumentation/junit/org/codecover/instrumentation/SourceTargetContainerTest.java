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

package org.codecover.instrumentation;

import static org.codecover.UtilsForTestingInstr.SOURCE;
import static org.codecover.UtilsForTestingInstr.TARGET;

import java.io.File;

import org.codecover.model.utils.file.SourceTargetContainer;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: SourceTargetContainerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SourceTargetContainerTest extends TestCase {

    public void testInstrumentationJob1() {
        File file1 = new File(SOURCE);
        File file2 = new File(TARGET);

        SourceTargetContainer job = new SourceTargetContainer(file1, file2);

        Assert.assertSame(file1, job.getSource());
        Assert.assertSame(file2, job.getTarget());
    }

    public void testInstrumentationJob2() {
        File file1 = null;
        File file2 = new File(TARGET);

        SourceTargetContainer job = new SourceTargetContainer(file1, file2);

        Assert.assertSame(file1, job.getSource());
        Assert.assertSame(file2, job.getTarget());
    }

    public void testInstrumentationJob3() {
        File file1 = new File(SOURCE);
        File file2 = null;

        SourceTargetContainer job = new SourceTargetContainer(file1, file2);

        Assert.assertSame(file1, job.getSource());
        Assert.assertSame(file2, job.getTarget());
    }

    public void testInstrumentationJob4() {
        File file1 = null;
        File file2 = null;

        SourceTargetContainer job = new SourceTargetContainer(file1, file2);

        Assert.assertSame(file1, job.getSource());
        Assert.assertSame(file2, job.getTarget());
    }
}
