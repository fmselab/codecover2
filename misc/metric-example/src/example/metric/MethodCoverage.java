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

package example.metric;

import java.util.Collections;
import java.util.List;
import java.util.Set;

import org.codecover.metrics.coverage.*;
import org.codecover.model.*;
import org.codecover.model.mast.*;
import org.codecover.model.utils.criteria.*;
import org.codecover.model.utils.*;
import org.codecover.model.extensions.*;

public class MethodCoverage extends AbstractCoverageMetric {

    private static final String CACHING_KEY = "example.metric.MethodCoverage";

    private static final String DESCRIPTION = "";

    private static final String NAME = "Method Coverage";

    private static MethodCoverage instance = new MethodCoverage();

    private MethodCoverage() {
        super(CACHING_KEY);
    }

    /**
     * This method returns an instance of MethodCoverage.
     * 
     * @return instance of MethodCoverage.
     */
    public static MethodCoverage getInstance() {
        return instance;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getDescription()
     */
    public String getDescription() {
        return DESCRIPTION;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getName()
     */
    public String getName() {
        return NAME;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.metrics.Metric#getRequiredCriteria()
     */
    public Set<Criterion> getRequiredCriteria() {
        return Collections.<Criterion>singleton(
            org.codecover.model.utils.criteria.StatementCoverage.getInstance());
    }
    
    public static final CoverageResult zeroOneResult = new CoverageResult(0, 1);
    public static final CoverageResult oneOneResult = new CoverageResult(1, 1);
    
    public CoverageResult getCoverageLocal(List<TestCase> testCases,
            Statement statement) {
        return CoverageResult.NULL;
    }

    public CoverageResult getCoverageLocal(List<TestCase> testCases, RootTerm term) {
        return CoverageResult.NULL;
    }
    
    public CoverageResult getCoverageLocal(List<TestCase> testCases,
                                           StatementSequence statements) {
        return CoverageResult.NULL;
    }
    
    public CoverageResult getCoverageLocal(final List<TestCase> testCases,
                                           HierarchyLevel level) {
        if (!level.getType().getInternalName().equals("method")) {
            return CoverageResult.NULL;
        }
        
        class Data {
            public boolean foundStatement = false;
            public boolean foundExecutedStatement = false;
        }
        final Data data = new Data();
        
        for (StatementSequence sequence : level.getSequences()) {
            sequence.accept(new Statement.DefaultVisitor() {
                    public void visit(BasicStatement statement) {
                        final CoverableItem item = statement.getCoverableItem();
                        if (item != null) {
                            data.foundStatement = true;
                            for (final TestCase testCase : testCases) {
                                if (testCase.getCoverageCount(item) > 0) {
                                    data.foundExecutedStatement = true;
                                }
                            }
                        }
                    }
                }, null, null, null, null, null);
        }
        
        if (data.foundStatement) {
            if (data.foundExecutedStatement) {
                return oneOneResult;
            } else {
                return zeroOneResult;
            } 
        } else {
            return CoverageResult.NULL;
        }
    }
    
    public CoverageResult getCoverageLocal(List<TestCase> testCases, Branch branch) {
        return CoverageResult.NULL;
    }

    public Set<Hint> getHints(List<TestCase> testCases,
                              Statement statement) {
        return noHints;
    }

    public Set<Hint> getHints(List<TestCase> testCases,
                              RootTerm term) {
        return noHints;
    }

    public Set<Hint> getHints(List<TestCase> testCases, StatementSequence statements) {
        return noHints;
    }

    public Set<Hint> getHints(List<TestCase> testCases, HierarchyLevel level) {
        return noHints;
    }

    public Set<Hint> getHints(List<TestCase> testCases, Branch branch) {
        return noHints;
    }
}
