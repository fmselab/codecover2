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

package org.codecover.model;

import java.io.File;
import java.util.Collections;
import java.util.Date;

import org.codecover.model.extensions.PluginManager;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.SimpleLogger;
import org.codecover.model.utils.criteria.Criterion;

/**
 * Very simple TSC Test
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: SimpleTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SimpleTest extends junit.framework.TestCase {

    /**
     * @throws java.lang.Exception
     */
    @Override
    protected void setUp() throws Exception {
        // Do nothing
    }
    
    /**
     * @throws Throwable
     */
    public void testSimple() throws Throwable {
        final MASTBuilder builder = new MASTBuilder(new SimpleLogger());
        final TestSessionContainer tsc = new TestSessionContainer(builder.createHierarchyLevel(builder.createLocationList(Collections.<Location>emptyList()), "root", builder.createLocationList(Collections.<Location>emptyList()), builder.createHierarchyLevelType("englishName", "internalName"), Collections.<HierarchyLevel>emptyList(), Collections.<StatementSequence>emptyList()), builder.getLogger(), Collections.<SourceFile>emptyList(), Collections.<Criterion>emptySet(), "someId", new Date());
        new File("test").mkdir();
        tsc.save("test/simpletest.xml");
        final MASTBuilder builder2 = new MASTBuilder(new SimpleLogger());
        TestSessionContainer.load(PluginManager.create(), builder2.getLogger(), builder2, "test/simpletest.xml");
    }
}
