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

package org.codecover.report.html;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Vector;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.model.TestCase;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.utils.Logger;
import org.codecover.report.highlighting.CodeHighlighting;
import org.codecover.report.highlighting.ExtractOfCodeFile;
import org.codecover.report.highlighting.TextLine;
import org.codecover.report.highlighting.annotation.DefaultCoverageAnnotation;
import org.codecover.report.highlighting.annotation.AnchorAnnotation;
import org.codecover.report.highlighting.annotation.CoverageAnnotation;
import org.codecover.report.highlighting.annotation.FatAnnotation;
import org.codecover.report.html.HTMLutils;

/**
 * Processes covered code to be used in a HTML report. It highlights the code
 * according to coverage which was achieved in given test cases.
 * 
 * @author Robert Hanussek, Michael Starzmann, Johannes Langauf
 * @version 1.1 ($Id: HTMLCodeHighlighting.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class HTMLCodeHighlighting extends CodeHighlighting {
    
    /**
     * Constructor set's the logger used for log-messages. Use null if you
     * want to drop any messages this class would print (log).
     *  
     * @param logger the logger to use or null to use none 
     */
    public HTMLCodeHighlighting(Logger logger) {
        super(logger);
    }
    
    /**
     * Adds HTML markup of what was, considering the metrics, being covered
     * by the testCases to the source code belonging to level and returns
     * this.
     * 
     * @param level     the HierarchyLevel of which the code is asked for
     * @param testCases the testcases of which the measured coverage should
     *                  be putted out 
     * @param metrics   the metrics deciding if something is covered or not
     * @return  source code with coverage markup
     */
    public List<ExtractOfCodeFile> getCoveredCode (HierarchyLevel level,
                                                   List<TestCase> testCases,
                                                   List<CoverageMetric> metrics) 
    { 
        return getCoveredCode(level, testCases, metrics, null);
    }
    
    /**
     * Adds HTML markup of what was, considering the metrics, being covered
     * by the testCases to the source code belonging to level and returns
     * this.
     * 
     * @param level     the HierarchyLevel of which the code is asked for
     * @param testCases the testcases of which the measured coverage should
     *                  be putted out 
     * @param metrics   the metrics deciding if something is covered or not
     * @param namedAnchors a name per HierarchyLevel, that is child of level and shall be set as anchor 
     * @return  source code with coverage markup
     */
    public List<ExtractOfCodeFile> getCoveredCode (HierarchyLevel level,
                                                   List<TestCase> testCases,
                                                   List<CoverageMetric> metrics,
                                HashMap<HierarchyLevel, String> namedAnchors)  
    {   
        List<ExtractOfCodeFile> coloredCode = new Vector<ExtractOfCodeFile>();
        getLogger().debug("Begin creating colored code-listing for "
                          + level.getName());
        getLogger().debug("Collects all highlightings");
        //TODO: Johannes: split list, one per file
        List<DefaultCoverageAnnotation> highlights = getHighlightsOfHierarchyLevel(
                                                                      level,
                                                                      testCases,
                                                                      metrics);
        Collections.sort(highlights, CoverageAnnotation.compareByStart);
        
        List<Location> loc = getValidLocationsOfLevel(level);
        for (Location l : loc) {
            getLogger().debug("  Begin creating colored code-listing of "
                    + level.getName() + " for file: "
                    + l.getFile());
            coloredCode.add(getMarkupedCode(l, highlights, childrenToAnchors(level, namedAnchors)));
        }
        return coloredCode;
    }
    
    private static void openCoverageTag(StringBuilder sb,
                                        CoverageAnnotation a) {
        sb.append("<span class=\"covered ");
        sb.append(a.getStatus().getText());
        sb.append("Covered ");
        sb.append(a.getMetric().getName().replace(' ', '_'));
        sb.append("\"");
        if (a.getNote() != null) {
            sb.append(" title=\"" + a.getNote() + "\"");
        }
        sb.append(">");
    }
    
    private static void closeCoverageTag(StringBuilder sb) {
        sb.append("</span>");
    }
    
    private static void putAnchorTag(StringBuilder sb, AnchorAnnotation a) {
        sb.append("<a name=\"" + a.getName() + "\"></a>");
    }
    
    /**
     * Produce HTML markup for an excerpt of a SourceFile.
     *
     * @param location
     * the range to annotate
     * @param annotations
     * all annotations in <code>location</code> and possibly more
     * @param anchorList
     * a list of named annotations as named references into the code
     * @return the code with HTML-Markup
     */
    private ExtractOfCodeFile getMarkupedCode(Location location,
                                         List<DefaultCoverageAnnotation> annotations,
                                         List<AnchorAnnotation> anchorList) {
        List<TextLine> textLines = new Vector<TextLine>();
        List<AnchorAnnotation> anchors = new Vector<AnchorAnnotation>();
        if (anchorList != null) {
            for (AnchorAnnotation a : anchorList) {
                if (a.getLocation().getFile() == location.getFile() && //FIXME: comparison by references: either must guarantee that no equivalent copy may be here or compare content
                    a.getStartOffset() >= location.getStartOffset() &&
                    a.getEndOffset() <= location.getEndOffset())
                {
                    anchors.add(a);
                }
            }
        }
        List<DefaultCoverageAnnotation> highlight = new Vector<DefaultCoverageAnnotation>();
        for (DefaultCoverageAnnotation h : annotations) {
            if (h.getLocation().getFile() == location.getFile() &&
                h.getLocation().getStartOffset() >= location.getStartOffset() &&
                h.getLocation().getEndOffset() <= location.getEndOffset())
            {
                highlight.add(h);
            }
        }
        List<DefaultCoverageAnnotation> openedCoverage = new Vector<DefaultCoverageAnnotation>();
        Collections.sort(highlight, FatAnnotation.compareByStart);
        Collections.sort(anchors, FatAnnotation.compareByStart);
        StringBuilder sb = new StringBuilder();
        long executions = -1;
        
        //extract text of the whole code file that contains the location
        String code = location.getFile().getContent();
        int lineNumber = countLines(code.substring(0, location.getStartOffset()));
        int position = location.getStartOffset();
        //go back to previous LineFeed
        while (   position > 0
               && ! (   code.charAt(position-1) == '\n'
                     || code.charAt(position-1) == '\r')) {
            position--;
        }
        //now we walk through the code till endOffset of location
        while (position < location.getEndOffset()) {
            //close opened coverage tags ending at this position
            while (   !openedCoverage.isEmpty()
                   && openedCoverage.get(0).getLocation().getEndOffset() == position) {
                closeCoverageTag(sb);
                openedCoverage.remove(0);
            }
            // set anchor tags of things starting at this position
            while (   !anchors.isEmpty()
                    && anchors.get(0).getLocation().getStartOffset() == position)
             {
                 putAnchorTag(sb, anchors.get(0));
                 anchors.remove(0);
             }
            // open coverage tags starting at this position
            while (   !highlight.isEmpty()
                    && highlight.get(0).getLocation().getStartOffset() == position)
             {
                 openCoverageTag(sb, highlight.get(0));
                 openedCoverage.add(highlight.get(0));
                 Collections.sort(openedCoverage, FatAnnotation.compareByEnd);
                 if (highlight.get(0).getExecutions() > executions) {
                     executions = highlight.get(0).getExecutions(); 
                 }
                 highlight.remove(0);
             }
            
            //now process the character at position
            if (code.charAt(position) == '\r' || code.charAt(position) == '\n') {
                /* process a line break*/
                // check for Windows line break which uses two characters...
                if (position + 1 < code.length()
                      && code.charAt(position) == '\r'
                      && code.charAt(position+1) == '\n')
                {
                    // ...therefore we eat one additional character, if found
                    position++;
                }
                
                //close tags opened here...
                for (int i = 0; i < openedCoverage.size(); i++) {
                    closeCoverageTag(sb);
                }
                
                textLines.add(new TextLine( lineNumber, executions, sb.toString()));
                
                /* start new line */ 
                lineNumber++;
                executions = -1;
                sb = new StringBuilder();
                
                /* ... and reopen tags in new line */
                for (CoverageAnnotation o : openedCoverage) {
                    openCoverageTag(sb, o);
                    if (o.getExecutions() > executions) {
                        executions = o.getExecutions(); 
                    }
                }
            } else {
                
                /* normal character: output it */
                //XXX would be nice if HTMLUtils escapes the whitespaces but
                //must happen something else than here
                if (   code.charAt(position) == ' '
                    && position + 1 < code.length()
                    && code.charAt(position + 1) == ' ') {
                    sb.append("&nbsp;");
                } else if (code.charAt(position) == '\t') {
                    sb.append("&nbsp;&nbsp;&nbsp;&nbsp;");
                } else {
                    sb.append(HTMLutils.escape(code.charAt(position)));
                }
            }
            position++;
        }
        textLines.add(new TextLine(lineNumber, executions, sb.toString()));
        return new ExtractOfCodeFile(location.getFile().getFileName(), textLines);
    }
}
