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

package org.codecover.eclipse.annotation.hotpath;

import org.codecover.eclipse.Messages;
import org.codecover.eclipse.annotation.EclPositionedAnnotation;
import org.codecover.report.highlighting.annotation.LineExecutionAnnotation;
import org.eclipse.jface.text.Position;

/**
 * Annotation for hot path showing execution count for one code line. The color
 * of the icon shown is defined in <code>LineExecutionImageProvider</code>.
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: EclLineExecutionAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see org.codecover.eclipse.annotation.hotpath.LineExecutionImageProvider
 */
public class EclLineExecutionAnnotation extends EclPositionedAnnotation {

    private static final String LINE_TEXT = Messages
            .getString("LineExecutionAnnotation.LINE_TEXT"); //$NON-NLS-1$

    private final LineExecutionAnnotation model;

    /**
     * > 0
     */
    private final long maxExecutions;

    /**
     * Create from model annotation.
     * 
     * @param a
     *            coverage information to generate presentation for, not null!
     * @param maxExecutions
     *            execution count to compare to, > 0, >= a.getExecutions()
     */
    public EclLineExecutionAnnotation(LineExecutionAnnotation a, long maxExecutions) {
        super(getAnnotationID(), new Position(a.getStartOffset(), 0));

        /* preconditions */
        final long execs = a.getExecutions();
        if (maxExecutions < execs) {
            throw new IllegalArgumentException(
                    "maxExecutions too small: < a.getExections()"); //$NON-NLS-1$
        }
        model = a;
        this.maxExecutions = maxExecutions;
    }

    private static String getAnnotationID() {
        return "org.codecover.eclipse.annotation.lineExecutionAnnotation"; //$NON-NLS-1$
    }

    @Override
    public String getText() {
        return String.format(LINE_TEXT, model.getLineNo(), model
                .getExecutions());
    }

    /**
     * @return the execution count of this line
     */
    public long getExecutions() {
        return model.getExecutions();
    }

    /**
     * The maximum execution count in the set of considered lines. This is
     * currently one source file. But it may be a larger set.
     * 
     * @return the maximum possible execution count
     */
    public long getMaxExecutions() {
        return maxExecutions;
    }

}
