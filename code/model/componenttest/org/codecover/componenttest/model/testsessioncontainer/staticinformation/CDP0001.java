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

package org.codecover.componenttest.model.testsessioncontainer.staticinformation;

import java.util.Set;

import org.codecover.model.*;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.utils.*;
import org.codecover.model.utils.criteria.*;

/**
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CDP0001.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CDP0001 extends junit.framework.TestCase {
    /**
     * Loads all the {@link TestSessionContainer}s in the multiplefile location
     * and checks, if their sets of criteria are correct.
     */
    public void testCDP0001() {
        String containerLocation = "../../qa/testdata/containers/multiplefile/";
        Logger logger = new SimpleLogger();
        MASTBuilder builder = new MASTBuilder(logger);

        try {
            // Load all the testSessionContainer
            TestSessionContainer noCriteriaContainer = TestSessionContainer
                    .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation
                            + "no-criteria.xml");
            TestSessionContainer statementContainer = TestSessionContainer
                    .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation + "statement.xml");
            TestSessionContainer branchContainer = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), 
                    logger, builder, containerLocation + "branch.xml");
            TestSessionContainer statementBranchContainer = TestSessionContainer
                    .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation
                            + "statement-branch.xml");
            TestSessionContainer loopContainer = TestSessionContainer.load(org.codecover.model.extensions.PluginManager.create(), 
                    logger, builder, containerLocation + "loop.xml");
            TestSessionContainer conditionContainer = TestSessionContainer
                    .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation + "condition.xml");
            TestSessionContainer allCriteriaContainer = TestSessionContainer
                    .load(org.codecover.model.extensions.PluginManager.create(), logger, builder, containerLocation
                            + "all-criteria.xml");

            Set<Criterion> criteriaList;
            // Check no-criteria
            criteriaList = noCriteriaContainer.getCriteria();
            assertFalse(criteriaList.contains(StatementCoverage.getInstance()));
            assertFalse(criteriaList.contains(LoopCoverage.getInstance()));
            assertFalse(criteriaList.contains(ConditionCoverage.getInstance()));
            assertFalse(criteriaList.contains(BranchCoverage.getInstance()));

            // Check statement
            criteriaList = statementContainer.getCriteria();
            assertTrue(criteriaList.contains(StatementCoverage.getInstance()));
            assertFalse(criteriaList.contains(LoopCoverage.getInstance()));
            assertFalse(criteriaList.contains(ConditionCoverage.getInstance()));
            assertFalse(criteriaList.contains(BranchCoverage.getInstance()));

            // Check branch
            criteriaList = branchContainer.getCriteria();
            assertFalse(criteriaList.contains(StatementCoverage.getInstance()));
            assertFalse(criteriaList.contains(LoopCoverage.getInstance()));
            assertFalse(criteriaList.contains(ConditionCoverage.getInstance()));
            assertTrue(criteriaList.contains(BranchCoverage.getInstance()));

            // Check statement-branch
            criteriaList = statementBranchContainer.getCriteria();
            assertTrue(criteriaList.contains(StatementCoverage.getInstance()));
            assertFalse(criteriaList.contains(LoopCoverage.getInstance()));
            assertFalse(criteriaList.contains(ConditionCoverage.getInstance()));
            assertTrue(criteriaList.contains(BranchCoverage.getInstance()));

            // Check loop
            criteriaList = loopContainer.getCriteria();
            assertFalse(criteriaList.contains(StatementCoverage.getInstance()));
            assertTrue(criteriaList.contains(LoopCoverage.getInstance()));
            assertFalse(criteriaList.contains(ConditionCoverage.getInstance()));
            assertFalse(criteriaList.contains(BranchCoverage.getInstance()));

            // Check condition
            criteriaList = conditionContainer.getCriteria();
            assertFalse(criteriaList.contains(StatementCoverage.getInstance()));
            assertFalse(criteriaList.contains(LoopCoverage.getInstance()));
            assertTrue(criteriaList.contains(ConditionCoverage.getInstance()));
            assertFalse(criteriaList.contains(BranchCoverage.getInstance()));

            // Check all-criteria
            criteriaList = allCriteriaContainer.getCriteria();
            assertTrue(criteriaList.contains(StatementCoverage.getInstance()));
            assertTrue(criteriaList.contains(LoopCoverage.getInstance()));
            assertTrue(criteriaList.contains(ConditionCoverage.getInstance()));
            assertTrue(criteriaList.contains(BranchCoverage.getInstance()));
        } catch (FileLoadException e) {
            // Fail, if an exception was thrown
            fail();
        }

    }
}
