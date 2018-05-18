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

package org.codecover.eclipse.actions;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.views.PickTestCaseView;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.model.TestCase;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.report.highlighting.CoverageStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.texteditor.ITextEditor;

//TODO: internationalization

/**
 * Display which test cases cover the current selection in the text editor.<p>
 * Using this class directly is not recommended as it's UI with model messages
 * is uncomfortable.
 *
 * @see PickTestCaseView
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: PickTestCaseActionDelegate.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class PickTestCaseActionDelegate extends
        PickCodeActionDelegate {

    private static final String PICK_TEST_CASE_STRING = Messages
            .getString("PickTestCaseActionDelegate.PICK_TEST_CASE_STRING"); //$NON-NLS-1$

    private static final String PICK_TEST_CASE_FOUND_INFO_MESSAGE = Messages
            .getString("PickTestCaseActionDelegate.PICK_TEST_CASE_FOUND_INFO_MESSAGE"); //$NON-NLS-1$

    private static final String PICK_TEST_CASE_FOUND_INFO_TITLE = Messages
            .getString("PickTestCaseActionDelegate.PICK_TEST_CASE_FOUND_INFO_TITLE"); //$NON-NLS-1$

    private static final String PICK_TEST_CASE_NOT_FOUND_WARNING_MESSAGE = Messages
            .getString("PickTestCaseActionDelegate.PICK_TEST_CASE_NOT_FOUND_WARNING_MESSAGE"); //$NON-NLS-1$

    private static final String PICK_TEST_CASE_NOT_FOUND_WARNING_TITLE = Messages
            .getString("PickTestCaseActionDelegate.PICK_TEST_CASE_NOT_FOUND_WARNING_TITLE"); //$NON-NLS-1$

    /**
     * @return a visitor that picks suitable MAST-Elements for this action
     */
    @Override
    protected PickVisitor getPickVisitor() {
        return new PickVisitor();
    }
    
    /**
     * Called when no MAST element is found
     */
    @Override
    protected void runNotSearched(ITextEditor editor) {
    }

    /**
     * Called when no MAST element is found
     */
    @Override
    protected void runNotFound(ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
        MessageDialog.openWarning(editor.getSite().getShell(),
                PICK_TEST_CASE_NOT_FOUND_WARNING_TITLE,
                PICK_TEST_CASE_NOT_FOUND_WARNING_MESSAGE);
    }
    
    /*
     * (non-Javadoc)
     * 
     * @see org.codecover.eclipse.actions.PickCodeActionDelegate#runFound(org.codecover.model.mast.MetaDataObject,
     *      org.codecover.eclipse.tscmanager.ActiveTSContainerInfo,
     *      org.eclipse.ui.texteditor.ITextEditor)
     */
    @Override
    protected void runFound(MetaDataObject pickedElement,
            ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
        TestSessionContainer tsc;
        List<TestCase> testCases;
        tsc = activeTSCInfo.getTestSessionContainer();
        testCases = new Vector<TestCase>(activeTSCInfo.getActiveTestCases());

        
        if (testCases.isEmpty()) {
            /* nothing selected to measure */
            return;
        }
        
        /* tsc is the active TSC, plugin our Plugin and editor and testCases
         * are the active testcases. */
        
        Set<CoverageMetric> coverageMetrics = MetricProvider.getAvailabeCoverageMetrics(CodeCoverPlugin.getDefault().getEclipsePluginManager().getPluginManager(), CodeCoverPlugin.getDefault().getLogger(), tsc.getCriteria());
        String matches = pickTestCasesString(testCases, coverageMetrics, tsc, pickedElement);

        MessageDialog.openInformation(editor.getSite().getShell(),
                PICK_TEST_CASE_FOUND_INFO_TITLE,
                PICK_TEST_CASE_FOUND_INFO_MESSAGE + matches);
    }
    
    /**
     * Get a list of test cases that cover <code>pickElement</code>.
     * 
     * @param testCases
     *            not null
     * @param metrics
     *            not null
     * @param tsc
     *            not null
     * @param pickElement
     *            Statement, RootTerm or Branch, not null
     * @return the list as a String with one test case per line, "" for nothing
     */
    public static String pickTestCasesString(List<TestCase> testCases,
            Set<CoverageMetric> metrics, TestSessionContainer tsc,
            MetaDataObject pickElement) {
        StringBuilder result = new StringBuilder(128);
        boolean first = true;

        Set<PickedTestCase> picked = pickTestCases(testCases,
                metrics, tsc, pickElement);

        for (PickedTestCase match: picked) {
            if (first) {
                /* first in list, don't need separator */
                first = false;
            } else {
                /* need this separator after last element */
                result.append(", \n"); //$NON-NLS-1$
            }
            
            //XXX: internationalize Metric name (m.getName() calls)
            result.append(String.format(PICK_TEST_CASE_STRING, match.getTestCase()
                    .getName(), match.getMetric().getName()));
        }
        
        return result.toString();
    }
    
    /**
     * A TestCase covered by a CoverageMetric.
     */
    public static class PickedTestCase {
        private final CoverageMetric metric;
        private final TestCase testCase;
        
        /**
         * Constructor
         * 
         * @param testCase
         *            the picked test case.
         * @param coveringMetric
         *            the {@link CoverageMetric} that covers the test case.
         */
        public PickedTestCase(TestCase testCase,
                CoverageMetric coveringMetric) {
            
            if (coveringMetric == null) {
                throw new NullPointerException("metric == null"); //$NON-NLS-1$
            }
            if (testCase == null) {
                throw new NullPointerException("testCase == null"); //$NON-NLS-1$
            }
            this.metric = coveringMetric;
            this.testCase = testCase;
        }
        
        @Override
        public String toString() {
            return testCase.getName() + "{by " //$NON-NLS-1$
                + metric.getName() + "}"; //$NON-NLS-1$
        }

        /**
         * @return the metric that covers the test case
         */
        public CoverageMetric getMetric() {
            return metric;
        }

        /**
         * @return the testCase
         */
        public TestCase getTestCase() {
            return testCase;
        }
    }
    
    /**
     * Get a list of test cases that cover
     * <code>pickElement</code>.
     * 
     * @param testCases not null
     * @param tsc not null
     * @param pickElement Statement, RootTerm or Branch, not null
     * 
     * @return the set of matching test cases
     */
    public static Set<PickedTestCase> pickTestCases(Iterable<TestCase> testCases,
            TestSessionContainer tsc, MetaDataObject pickElement) {
        Set<CoverageMetric> metrics = MetricProvider.getAvailabeCoverageMetrics(CodeCoverPlugin.getDefault().getEclipsePluginManager().getPluginManager(), CodeCoverPlugin.getDefault().getLogger(), tsc.getCriteria());
        return pickTestCases(testCases, metrics, tsc, pickElement);
    }
    /**
     * Get a list of test cases that cover
     * <code>pickElement</code>.
     * 
     * @param testCases not null
     * @param metrics not null
     * @param tsc not null
     * @param pickElement Statement, RootTerm or Branch, not null
     * 
     * @return the set of matching test cases
     */
    public static Set<PickedTestCase> pickTestCases(Iterable<TestCase> testCases,
            Set<CoverageMetric> metrics, TestSessionContainer tsc,
            MetaDataObject pickElement) {
        Set<PickedTestCase> result =
            new HashSet<PickedTestCase>();


        CodeCoverPlugin.getDefault().getLogger().debug("picked: " + pickElement); //$NON-NLS-1$

        if (pickElement instanceof ConditionalStatement) {
            //check coverage of all branches together with this match
            for (TestCase tc : testCases) {
                for (CoverageMetric m: metrics) {
                    CoverageResult cov;
                    cov = m.getCoverageLocal(list(tc), (ConditionalStatement) pickElement);
                    int coveredItems = cov.getCoveredItems();
                    int totalItems = cov.getTotalItems();
                    for (Branch child: ((ConditionalStatement)pickElement).getBranches()) {
                        cov = m.getCoverageLocal(list(tc), child);
                        coveredItems += cov.getCoveredItems();
                        totalItems += cov.getTotalItems();
                    }
                    CoverageStatus status;
                    status = CoverageStatus.calcCoverageStatus(coveredItems,
                            totalItems);
                    if (status == CoverageStatus.FULLY) {
                        result.add(new PickedTestCase(tc, m));
                    }
                }
            }
        } else if (pickElement instanceof Statement) {
            for (TestCase tc : testCases) {
                for (CoverageMetric m: metrics) {
                    CoverageResult cov;
                    cov = m.getCoverageLocal(list(tc), (Statement) pickElement);
                    CoverageStatus status = CoverageStatus.calcCoverageStatus(cov);
                    if (status == CoverageStatus.FULLY) {
                        result.add(new PickedTestCase(tc, m));
                    }
                }
            }
        } else if (pickElement instanceof RootTerm) {
            for (TestCase tc : testCases) {
                for (CoverageMetric m: metrics) {
                    CoverageResult cov;
                    /* n.b. there is no local coverage for root terms, but
                     * we don't want too much detail already in the
                     * boolean analyzer. Hence cummulate per RootTerm. */
                    cov = m.getCoverage(list(tc), (RootTerm) pickElement);
                    CoverageStatus status = CoverageStatus.calcCoverageStatus(cov);
                    if (status == CoverageStatus.FULLY) {
                        result.add(new PickedTestCase(tc, m));
                    }
                }
            }
        } else if (pickElement instanceof Branch) {
            for (TestCase tc : testCases) {
                for (CoverageMetric m: metrics) {
                    CoverageResult cov;
                    cov = m.getCoverageLocal(list(tc), (Branch) pickElement);
                    CoverageStatus status = CoverageStatus.calcCoverageStatus(cov);
                    if (status == CoverageStatus.FULLY) {
                        result.add(new PickedTestCase(tc, m));
                    }
                }
            }
        } else {
            //XXX: allow more? - current Metrics don't need more.
            //to change also add them in instance of PickVisitor
            CodeCoverPlugin.getDefault().getLogger().debug(
                    "refusing to calculate coverage for unexpected class: " //$NON-NLS-1$
                    + pickElement.getClass().getName());
        }
        
        return result;
    }

    private static List<TestCase> list(TestCase tc) {
        List<TestCase> result = new Vector<TestCase>(1);
        result.add(tc);
        return result;
    }
    
    /**
     * Traverses a given test session container to find an individually coverable element
     * from its location.
     */
    private static class PickVisitor extends PickCodeActionDelegate.PickVisitor {
        
        @Override
        protected void traversePostfix(TestSessionContainer tsc) {
            //ignore HierarchyLevels and BooleanTerms
            tsc.getCode().accept(null, null, null, this, null, this, null, null, null);
        }
        
        /*
         * run update match functions on relevant locations to find best match
         */
        @Override
        public void visit(RootTerm term) {
            super.visit(term);
            updateMatch(term);
        }

        @Override
        public void visit(Branch branch) {
            super.visit(branch);
            updateMatch(branch);
        }

        @Override
        public void visit(BasicStatement statement) {
            super.visit(statement);
            updateMatch(statement);
        }

        @Override
        public void visit(ConditionalStatement statement) {
            super.visit(statement);
            updateMatch(statement);
        }
        
        @Override
        public void visit(LoopingStatement statement) {
            super.visit(statement);
            updateMatch(statement);
        }

        @Override
        public void visit(StatementSequence sequence) {
            super.visit(sequence);
            updateMatch(sequence);
        }
    }
}
