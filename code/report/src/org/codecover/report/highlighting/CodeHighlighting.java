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

package org.codecover.report.highlighting;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.metrics.coverage.StatementCoverage;
import org.codecover.metrics.coverage.CoverageMetric.Hint;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestCase;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.QuestionMarkOperator;
import org.codecover.model.mast.QuestionMarkOperatorExpression;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;
import org.codecover.model.utils.Logger;
import org.codecover.report.highlighting.annotation.AnchorAnnotation;
import org.codecover.report.highlighting.annotation.CoverageAnnotation;
import org.codecover.report.highlighting.annotation.DefaultAnchorAnnotation;
import org.codecover.report.highlighting.annotation.DefaultCoverageAnnotation;
import org.codecover.report.highlighting.annotation.DefaultLineExecutionAnnotation;
import org.codecover.report.highlighting.annotation.LineExecutionAnnotation;

/**
 * Highlights the code according to coverage which was achieved in given test
 * cases. A presentation independent format is generated based on
 * {@link org.codecover.report.highlighting.annotation.Annotation}.
 *
 * @author Robert Hanussek, Michael Starzmann, Johannes Langauf
 * @version 1.0 ($Id: CodeHighlighting.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class CodeHighlighting {

    /**
     * The logger.
     */
    private Logger logger;


    /**
     * Constructor set's the logger used for log messages.
     *
     * @param logger the logger to use
     */
    public CodeHighlighting(Logger logger) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }

        this.logger = logger;
    }

    protected Logger getLogger() {
        return this.logger;
    }

    // not every level has a location. A package, e.g. has none. In such a case,
    // we take the locations of every child, that would be classes, which have
    // locations
    protected static List<Location> getValidLocationsOfLevel(HierarchyLevel level) {
        if (level.getLocation() == null || level.getLocation().getLocations().size() == 0) {
            List<Location> result = new Vector<Location>();
            for (HierarchyLevel l : level.getChildren()) {
                result.addAll(getValidLocationsOfLevel(l));
            }
            return result;
        } else {
            return level.getLocation().getLocations();
        }
    }


    protected static List<AnchorAnnotation> childrenToAnchors (HierarchyLevel level, HashMap<HierarchyLevel, String> namedAnchors) {
        List<AnchorAnnotation> result = new Vector<AnchorAnnotation>();
        if (namedAnchors != null) {
            if (   level.getLocation() != null
                && level.getLocation().getLocations() != null
                && level.getLocation().getLocations().size() > 0) {
                    result.add(new DefaultAnchorAnnotation(level.getLocation().getLocations().get(0), namedAnchors.get(level)));
                }
                for (HierarchyLevel child : level.getChildren()) {
                    result.addAll(childrenToAnchors(child, namedAnchors));
                }
        }
        return result;
    }

    /*
     * Methods to generate line by line coverage.
     */

    /**
     * Generate a List with execution counters for all lines of file.
     * <p>Line i can be found at position i - 1 in the returned List.
     *
     * @param executionInformation
     * annotations in file which include all the execution counters. Must be
     * sorted by start offset.
     * @param code
     * the file to split into lines
     * @return
     * the list of lines, in order, not <code>null</code>
     */
    public HighlightedSnippet generateLineAnnotationsByFile(
            List<CoverageAnnotation> executionInformation,
            final String code) {
        Vector<LineExecutionAnnotation> result;
        result = new Vector<LineExecutionAnnotation>(100);

        int lineNumber = 1; //number of first line
        int position = 0; //offset of first character
        long maxExecutions = 0; //to be returned
        List<CoverageAnnotation> openedAnnotations;
        openedAnnotations = new Vector<CoverageAnnotation>(30);

        /* sort input by start offset */
        Iterator<CoverageAnnotation> annoationIterator = executionInformation.iterator();
        CoverageAnnotation nextExecutionInformation;
        if (annoationIterator.hasNext()) {
            nextExecutionInformation = annoationIterator.next();
        } else {
            nextExecutionInformation = null;
        }

        do {

            long executions = -1; /* execution count of this line */
            int lineStart = position;

            /* keep only opened annotations in openedAnnotations */
            List<CoverageAnnotation> nextOpenedAnnotations = null;
            if (!openedAnnotations.isEmpty()) {
                nextOpenedAnnotations = new Vector<CoverageAnnotation>(30);
                for (CoverageAnnotation a : openedAnnotations) {
                    /*HOOK: new annotaion added */
                    if (a.getEndOffset() > position) {
                        if (executions < a.getExecutions()) {
                                executions = a.getExecutions();
                        }
                        nextOpenedAnnotations.add(a);
                    }
                }
                openedAnnotations = nextOpenedAnnotations;
            }

            /* HOOK: got opened Annotation at line start now */

            /* move position to the offset after the last character of this line */
            position = nextLinebreak(code, position);

            /* add new annotations up to position */
            while (nextExecutionInformation != null
                   && nextExecutionInformation.getStartOffset() < position) {
                /*HOOK: new annotation added */
                if (executions < nextExecutionInformation.getExecutions()) {
                        executions = nextExecutionInformation.getExecutions();
                }
                openedAnnotations.add(nextExecutionInformation);
                if (annoationIterator.hasNext()) {
                    nextExecutionInformation = annoationIterator.next();
                } else {
                    nextExecutionInformation = null;
                }
            }

            /* all annotations of this the line before position are in openedAnnotations */

            /* HOOK: information about this line is complete */
            if (maxExecutions < executions) {
                maxExecutions = executions;
            }

            LineExecutionAnnotation lea = new DefaultLineExecutionAnnotation(code, lineStart,
                    position, lineNumber, executions);

            /* assertion:
            for (ICoverageAnnotation a : openedAnnotations) {
                if (!a.intersects(lea)) {
                    //may not reach this
                }
            }*/

            result.add(lea);

            /* step to start of next line */
            position = skipLineBreak(code, position);
            lineNumber++;
        } while (position != -1);

        return new HighlightedSnippet("", result, maxExecutions);
    }


    /**
     * Step forward to the next line break in a String.
     * @param s
     * the String
     * @param base
     * the current position in s, < s.length(), >= 0
     * @return
     * the offset of the first character of the next line break, s.length() for the last line
     */
    private static int nextLinebreak(final String s, final int base) {
        int pos = base;

        /*if (base < s.length()) {
            pos = base + 1; //the next line can't be at the first character
        } else {
            return base;
        }*/

        while (pos < s.length()) {
            //TODO: check performance
            if (s.charAt(pos) == '\r' || s.charAt(pos) == '\n') {
                return pos;
            }
            pos++;
        }

        return pos;
    }

    /**
     * Step one line break forward in a String.
     * @param s
     * the String
     * @param base
     * the current position in s, on a line break
     * @return
     * the offset of the first character of the next line, -1 if this is the last line
     */
    private static int skipLineBreak(final String s, final int base) {
        int pos = base;

        if (pos < s.length()) {
            if (s.charAt(pos) == '\r') {
                pos++;
                // Windows uses the combination \r\n for ONE
                // linebreak, so as we found an \r, eat the
                // next character if it's an \n
                if (pos < s.length() && s.charAt(pos) == '\n') {
                    pos++;
                }
                return pos;
            } else if (s.charAt(pos) == '\n') {
                return pos + 1;
            }
        }
        return -1;
    }

    /*
     * Methods to generate Coverage Annotations from Hierarchy Levels
     */

    @Deprecated
    public List<DefaultCoverageAnnotation> getHighlightsOfHierarchyLevel(
            HierarchyLevel level,
            List<TestCase> testCases,
            List<CoverageMetric> metrics) {
        if (level == null) {
            throw new NullPointerException("level == null");
        }
        if (testCases == null) {
            throw new NullPointerException("testCases == null");
        }
        if (metrics == null) {
            throw new NullPointerException("metrics == null");
        }

        final List<DefaultCoverageAnnotation> highlights = new ArrayList<DefaultCoverageAnnotation>();

        Map<SourceFile,List<CoverageAnnotation>> result;
        result = annotateCoverage(level, testCases, metrics);

        for (List<CoverageAnnotation> annotations : result.values()) {
            //XXX: will break soon, as other annotations are used, then remove whole method
            try {
                for (CoverageAnnotation annotation : annotations) {
                    highlights.add((DefaultCoverageAnnotation)annotation);
                }
            } catch (ClassCastException e) {
                this.logger.error("CodeHighlighting.getHighlightsOfHierarchyLevel() is broken and returned too little annotations. ask a developer to remove it cleanly."); //
                return highlights;
            }
        }

        return highlights;
    }

    /**
     * Produce a file oriented model of the coverage information. All local
     * coverage information found in the node and it's descendants is
     * converted into coverage annotations.
     *
     * @param node
     * the node to create annotations for
     * @param testCases
     * the testCases, logically merged into one, to create annotations from
     * @param metrics
     * the metrics to create annotations for
     * @param file
     * the file to annotate
     * @return
     * the annotations for <code>file</code>
     */
    public List<CoverageAnnotation> annotateCoverage(
            HierarchyLevel node,
            List<TestCase> testCases,
            List<CoverageMetric> metrics,
            final SourceFile file) {
        if (node == null) {
            throw new NullPointerException("node == null");
        }
        if (testCases == null) {
            throw new NullPointerException("testCases == null");
        }
        if (metrics == null) {
            throw new NullPointerException("metrics == null");
        }
        if (file == null) {
            throw new NullPointerException("file == null");
        }

        MASTBuilder builder = new MASTBuilder(this.logger);
        Location location;
        location = builder.createLocation(file, 0, file.getContent().length());
        List<Location> filter;
        filter = Collections.singletonList(location);
        Map<SourceFile,List<CoverageAnnotation>> result;
        result = annotateCoverage(node, testCases, metrics, filter, true);



        return result.get(file);
    }


    /**
     * Produce a file oriented model of the coverage information. All local
     * coverage information found in the node and it's descendants is
     * converted into coverage annotations.
     *
     * @param node
     * the node to create annotations for
     * @param testCases
     * the testCases, logically merged into one, to create annotations from
     * @param metrics
     * the metrics to create annotations for
     * @return
     * the annotations for <code>file</code>
     */
    public Map<SourceFile,List<CoverageAnnotation>> annotateCoverage(
            HierarchyLevel node,
            List<TestCase> testCases,
            List<CoverageMetric> metrics) {
        return annotateCoverage(node, testCases, metrics, null, false);
    }

    /**
     * Produce a file oriented model of the coverage information. All local coverage information found in the
     * node and it's descendants is converted into coverage annotations.
     *
     * @param node
     *            the node to create annotations for
     * @param testCases
     *            the testCases, logically merged into one, to create annotations from
     * @param metrics
     *            the metrics to create annotations for
     * @param filter
     *            list of Locations surrounding all annotations, if filterOn
     * @param filterOn
     *            true to enable given filter
     * @return the annotations separated by SourceFile
     */
    public Map<SourceFile, List<CoverageAnnotation>> annotateCoverage(
            HierarchyLevel node,
            List<TestCase> testCases,
            List<CoverageMetric> metrics,
            final List<Location> filter,
            final boolean filterOn) {
        if (node == null) {
            throw new NullPointerException("level == null");
        }
        if (testCases == null) {
            throw new NullPointerException("testCases == null");
        }
        if (metrics == null) {
            throw new NullPointerException("metrics == null");
        }
        if (filterOn) {
            if (filter == null) {
                throw new NullPointerException("filter == null, but filter is on");
            }
        }


        final Map<SourceFile, List<CoverageAnnotation>> annotatedFiles;
        annotatedFiles = new HashMap<SourceFile, List<CoverageAnnotation>>();

        for (final CoverageMetric metric : metrics) {
            metric.accept(testCases, node, new CoverageMetric.DefaultPreMetricVisitor(),
                new CoverageMetric.PostMetricVisitor() {
                    private void process(List<Location> locations, CoverageResult result, Set<Hint> hints) {
                        if (result.isNull() && hints.isEmpty()) {
                            return;
                        }

                        // calculate coverage status for Annotation
                        CoverageStatus status = CoverageStatus.calcCoverageStatus(result);
                        // add missing coverage
                        if (result.isNull()) {
                            // add coverage that is wrapped inside a Hint to
                            // show this element as an annotation (they need a
                            // measured coverage)

                            for (final Hint hint : hints) {
                                if (hint instanceof CoverageMetric.CoverageWrapper) {
                                    // unwrap status for ConditionalStatement
                                    // currently the only case where it is
                                    // needed
                                    final CoverageMetric.CoverageWrapper cWrap = (CoverageMetric.CoverageWrapper)hint;
                                    status = CoverageStatus.calcCoverageStatus(cWrap.getResult());
                                    break;
                                }
                            }
                        }

                        long executions = -1;
                        for (final Hint hint : hints) {
                            if (hint instanceof StatementCoverage.ExecutionsHint) {
                                executions = ((StatementCoverage.ExecutionsHint)hint).getNumberOfExecutions();
                                break;
                            }
                        }

                        if (status != CoverageStatus.NONE) {
                            // prepared all attributes for coverage annotation

                            for (final Location location : locations) {
                                if (location != null) {
                                    // got something to annotate
                                    if (filterOn) {
                                        //check filter
                                        for (Location outer : filter) {
                                            if (outer.contains(location)) {
                                                //filter matches
                                                addAnnotation(annotatedFiles, location,
                                                        result, status, metric, hints,
                                                        executions);
                                                break; //add only once
                                            }
                                        }
                                    } else {
                                        // filtering off
                                        addAnnotation(annotatedFiles, location,
                                                result, status, metric, hints,
                                                executions);
                                    }
                                }
                            }
                        } else {
                            // metric gave invalid result

                            throw new RuntimeException(
                                    "Got invalid CoverageStatus (NONE without"
                                    + " a CoverageWrapper) from Visitor of"
                                    + " Metric " + metric.getName()
                                    + "for code at:" + locations);
                        }
                    }

                    public void visit(HierarchyLevel level, CoverageResult result, Set<Hint> hints) {
                        process(level.getHeader().getLocations(), result, hints);
                    }

                    public void visit(BasicStatement statement, CoverageResult result, Set<Hint> hints) {
                        process(statement.getLocation().getLocations(), result, hints);
                    }

                    public void visit(ConditionalStatement statement, CoverageResult result, Set<Hint> hints) {
                        process(Collections.singletonList(statement.getKeyword()), result, hints);
                    }

                    public void visit(LoopingStatement statement, CoverageResult result, Set<Hint> hints) {
                        process(Collections.singletonList(statement.getKeyword()), result, hints);
                    }

                    public void visit(StatementSequence statements, CoverageResult result, Set<Hint> hints) {
                        process(statements.getLocation().getLocations(), result, hints);
                    }

                    public void visit(Branch branch, CoverageResult result, Set<Hint> hints) {
                        process(branch.getDecision().getLocations(), result, hints);
                    }

                    public void visit(RootTerm term, CoverageResult result, Set<Hint> hints) {
                        process(term.getTerm().getLocation().getLocations(), result, hints);
                    }

                    public void visit(BasicBooleanTerm term, RootTerm rootTerm, CoverageResult result, Set<Hint> hints) {
                        process(term.getLocation().getLocations(), result, hints);
                    }

                    public void visit(OperatorTerm term, RootTerm rootTerm, CoverageResult result, Set<Hint> hints) {
                        process(term.getLocation().getLocations(), result, hints);
                    }

					public void visit(QuestionMarkOperator qmo,
							CoverageResult result, Set<Hint> hints) {
                        process(qmo.getLocation().getLocations(), result, hints);
						
					}

					public void visit(QuestionMarkOperatorExpression qmoe,
							CoverageResult result, Set<Hint> hints) {
                        process(qmoe.getLocation().getLocations(), result, hints);
						
					}

					public void visit(
							SynchronizedStatement synchronizedStatement,
							CoverageResult result, Set<Hint> hints) {
                        process(synchronizedStatement.getLocation().getLocations(), result, hints);
						
					}
            });
        }

        return annotatedFiles;
    }

    private void addAnnotation(Map<SourceFile,List<CoverageAnnotation>> target,
            Location location,
            CoverageResult result,
            CoverageStatus status,  // could be derived from other arguments
            CoverageMetric metric,
            Set<Hint> hints,
            long executions         // could be derived from other arguments
            ) {
        List<CoverageAnnotation> highlights;
        highlights = target.get(location.getFile());
        if (highlights == null) {
            highlights = new ArrayList<CoverageAnnotation>();
            target.put(location.getFile(), highlights);
        }

        CoverageAnnotation a =  createCoverageAnnotation(location, result, status,
                metric, hints, executions);
        highlights.add(a);
    }

    /**
     * Hook to create annotations. All CoverageAnnotations are created in
     * {@link CodeHighlighting} created with this method.
     * <p>
     * Arguments may not be null.
     */
    protected CoverageAnnotation createCoverageAnnotation(Location location,
            CoverageResult result,
            CoverageStatus status,  // could be derived from other arguments
            CoverageMetric metric,
            Set<Hint> hints,
            long executions         // could be derived from other arguments
            ) {
        return new DefaultCoverageAnnotation(location, status, metric, hints, executions);
    }

    protected static int countLines(String text) {
        int counter = 1;
        char[] chars = text.toCharArray();

        for (int i = 0; i < chars.length; i++) {
            if (chars[i] == '\r') {
                counter++;
                // Windows uses the combination \r\n for ONE
                // linebreak, so as we found an \r, eat the
                // next character if it's an \n
                if (i+1 < chars.length && chars[i+1] == '\n') {
                    i++;
                }
            } else if (chars[i] == '\n') {
                counter++;
            }
        }

        return counter;
    }
}
