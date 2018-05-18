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
package org.codecover.specials;

import static org.codecover.UtilsForTestingJava.TARGET;
import static org.codecover.UtilsForTestingJava.instrumentAndCompile;
import static org.codecover.UtilsForTestingJava.runJava;

import java.io.File;
import java.util.LinkedList;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.measurement.CoverageResultLog;
import org.codecover.instrumentation.measurement.NullCoverageLog;
import org.codecover.instrumentation.measurement.parser.CoverageLogParser;
import org.codecover.model.mast.HierarchyLevel;

/**
 * @author Christoph Müller<br>
 * <br>
 * Test the loop variants.
 *
 * @version 1.0 ($Id: LoopTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LoopTest extends TestCase {
    public void testLoopTest() throws Exception {
        HierarchyLevel clazz = instrumentAndCompile("test1", "LoopTest");
        String returnValue = runJava("org.codecover.instrumentation.java15.test.test1.LoopTest",
                                     new String[]{"-Dorg.codecover.coverage-log-file=" + TARGET + "loop_test.clf"},
                                     new String[]{"ARGUMENT"});
        Assert.assertEquals("ARGUMENT", returnValue);

        final LinkedList<IDValueContainer> IDsAndValues = new LinkedList<IDValueContainer>();
        IDsAndValues.addLast(new IDValueContainer("L1-0", 1));
        IDsAndValues.addLast(new IDValueContainer("L2-1", 1));
        IDsAndValues.addLast(new IDValueContainer("L3-2", 1));
        IDsAndValues.addLast(new IDValueContainer("L4-2", 1));
        IDsAndValues.addLast(new IDValueContainer("L5-1", 1));
        IDsAndValues.addLast(new IDValueContainer("L6-2", 1));
        IDsAndValues.addLast(new IDValueContainer("L7-2", 1));
        IDsAndValues.addLast(new IDValueContainer("L8-0", 1));
        IDsAndValues.addLast(new IDValueContainer("L9-1", 1));
        IDsAndValues.addLast(new IDValueContainer("L10-2", 1));
        IDsAndValues.addLast(new IDValueContainer("L11-2", 1));

        CoverageLogParser coverageLogParser = new CoverageLogParser(new File(TARGET + "loop_test.clf"));
        CoverageResultLog coverageResultLog = new NullCoverageLog() {
            public void passCounter(String counterID, long counterValue) {
                if (counterID.startsWith("L")) {
                    IDValueContainer container = IDsAndValues.poll();
                    Assert.assertEquals(container.ID, counterID);
                    Assert.assertEquals(container.value, counterValue);
                }
            }
        };

        coverageLogParser.CompilationUnit(coverageResultLog, null);
        Assert.assertTrue(IDsAndValues.isEmpty());
    }
    
    private static class IDValueContainer {
        String ID;
        long value;

        IDValueContainer(String id, long value) {
            this.ID = id;
            this.value = value;
        }
    }
}
