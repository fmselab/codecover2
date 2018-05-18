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

package org.codecover.report.highlighting.annotation;

import java.util.Set;

import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageMetric.EnglishTextHint;
import org.codecover.metrics.coverage.CoverageMetric.Hint;
import org.codecover.model.mast.Location;
import org.codecover.report.highlighting.CoverageStatus;

/**
 * Annotation about the coverage status of a MAST element.
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultCoverageAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultCoverageAnnotation extends DefaultAnnotation implements CoverageAnnotation {

    //TODO: light version (without SourceFile)
    
    //private MASTELEMENT ? - with set of test cases it could derive note and status.
    private final CoverageStatus status;
    private final CoverageMetric metric;
    private final Set<Hint> hints;
    
    /**
     * How often this Annotation's coverage criterion was fullfilled.
     */
    private final long executions;
    
    public DefaultCoverageAnnotation(Location location,
                             CoverageStatus status,
                             CoverageMetric metric,
                             Set<Hint> hint,
                             long executions) {
        super(location);
        
        this.status = status;
        this.metric = metric;
        this.hints = hint;
        this.executions = executions;
        
        /* check preconditions: */
        //TODO: comment these checks out to avoid performance penalty
        checkCoverage();
    }
    
    protected void checkCoverage() {
        /* check parent assertions */
        super.check();
        
        /* local assertions */
        if (status == CoverageStatus.NONE) {
            throw new IllegalArgumentException(
                    "CoverageAnnotation without Coverage (status==NONE).");
        }
        if (getStatus() == null) {
            throw new NullPointerException("status is null");
        }
        if (getMetric() == null) {
            throw new NullPointerException("metric is null");
        }
        if (getHints() == null) {
            throw new NullPointerException("hints is null");
        }
    }
    
    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ICoverageAnnotation#getExecutions()
     */
    public long getExecutions() {
        return executions;
    }

    public boolean hasExecutions() {
        return getExecutions() == -1;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ICoverageAnnotation#getMetric()
     */
    public CoverageMetric getMetric() {
        return metric;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ICoverageAnnotation#getNote()
     */
    public String getNote() {
        final StringBuilder noteBuilder = new StringBuilder();
        boolean hasNote = false;
        
        for (final Hint hint : getHints()) {
            final String hintString = hintToString(hint);
            if (hintString != null) {
                if (hasNote) {
                    noteBuilder.append("\n");
                }
                noteBuilder.append(hintString);
                hasNote = true;
            }
        }
        
        final String note = hasNote ? noteBuilder.toString() : null;
        return note;
    }
    
    private String hintToString(final Hint hint) {
        if (hint instanceof EnglishTextHint) {
            final EnglishTextHint hint2 = (EnglishTextHint)hint;
            return hint2.toEnglishText();
        } else {
            return null;
        }
    }
    
    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.ICoverageAnnotation#getStatus()
     */
    public CoverageStatus getStatus() {
        return status;
    }
    
    @Override
    public String toString() {
        return super.toString() + " with {status=" + getStatus().getText()
        + ", executions="+ getExecutions() + "}";
    }

    public Set<Hint> getHints() {
        return hints;
    }
}
