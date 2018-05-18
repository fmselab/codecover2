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

package org.codecover.instrumentation.java15.counter;

import java.io.StringWriter;
import java.util.UUID;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.java.measurement.CoverageResultLogFile;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: CounterIDManagerTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class CounterIDManagerTest extends TestCase {

    /**
     * Test method for {@link org.codecover.instrumentation.java15.counter.CounterIDManager#createInnerClassName(java.lang.String)}.
     */
    public void testCreateInnerClassName1() {
        String sourceFileName;
        String fullSourceFileName;
        CounterIDManager counterIDManager;

        sourceFileName = "ALongClassName.java";
        fullSourceFileName = "org.gbt2" + sourceFileName;
        counterIDManager = new CounterIDManager(sourceFileName, fullSourceFileName,
                new StringWriter(), CoverageResultLogFile.class, 
                UUID.randomUUID().toString());

        Assert.assertEquals("CodeCoverCoverageCounter$12m4p2xdd3i4eqp2u4li55o4lp8h9d",
                counterIDManager.getInnerClassName());

        sourceFileName = "Main.java";
        fullSourceFileName = "org.gbt2" + sourceFileName;
        counterIDManager = new CounterIDManager(sourceFileName, fullSourceFileName,
                new StringWriter(), CoverageResultLogFile.class, 
                UUID.randomUUID().toString());

        Assert.assertEquals("CodeCoverCoverageCounter$8d8uuo7joycbc1",
                counterIDManager.getInnerClassName());

        sourceFileName = "ASDFajsdfakjsfsadjfhsadljhflkjsahdflökjsödfjaoifovinaimremisakmcioamnervpnaosdafjaoipwevmaiomeorhgojdsaäjaö.java";
        fullSourceFileName = "org.gbt2" + sourceFileName;
        counterIDManager = new CounterIDManager(sourceFileName, fullSourceFileName,
                new StringWriter(), CoverageResultLogFile.class, 
                UUID.randomUUID().toString());

        Assert.assertEquals("CodeCoverCoverageCounter$1j252me1k4xmk4t2t4ox759imnecwabxbqysut9kmme9bscv2nx5hl9z77lzloqi7jui0r371qvbqr8n0det82w3z5brq7blot011v9vve5zjazkvh4ivrhz3vuycis1j771cy8n3ogsnrql4liibxbyesnkho1fh8i1m0ds9jv46r97pfmp",
                counterIDManager.getInnerClassName());

        sourceFileName = "Main.java";
        fullSourceFileName = "org.gbt2" + sourceFileName;
        counterIDManager = new CounterIDManager(sourceFileName, fullSourceFileName,
                new StringWriter(), CoverageResultLogFile.class, 
                UUID.randomUUID().toString());

        Assert.assertEquals("CodeCoverCoverageCounter$8d8uuo7joycbc1",
                counterIDManager.getInnerClassName());
    }
}
