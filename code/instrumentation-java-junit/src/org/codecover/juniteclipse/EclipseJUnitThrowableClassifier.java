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
package org.codecover.juniteclipse;

import org.codecover.junit.HelperMethods;
import org.eclipse.jdt.internal.junit.runner.DefaultClassifier;
import org.eclipse.jdt.internal.junit.runner.IClassifiesThrowables;

/**
 * A {@link IClassifiesThrowables}, whose
 * {@link IClassifiesThrowables#getTrace(Throwable)} is more readable and
 * can be used for {JUnitEclipseFailureContainer#writeFailure(StringBuffer)}.<br>
 * <br>
 * It has the structure:
 * <pre>
 * : classname
 * message
 * stacktrace
 * </pre>
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: EclipseJUnitThrowableClassifier.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclipseJUnitThrowableClassifier extends DefaultClassifier implements
        IClassifiesThrowables {

    private final IClassifiesThrowables delegateClassifiesThrowables;

    /**
     * Constructor.
     * 
     * @param classifiesThrowables {@link DefaultClassifier#DefaultClassifier(String)}
     */
    public EclipseJUnitThrowableClassifier(IClassifiesThrowables classifiesThrowables) {
        super();
        this.delegateClassifiesThrowables = classifiesThrowables;
    }

    /**
     * Analyzes a {@link Throwable}, to get a String that can be used for
     * {@link JUnitEclipseFailureContainer#writeFailure(StringBuffer)}.
     * 
     * @param t The {@link Throwable}, whose stack trace should be parsed
     * 
     * @return A stack trace String.
     */
    @Override
    public String getTrace(Throwable t) {
        StringBuilder builder = new StringBuilder();
        builder.append(": ");
        builder.append(t.getClass().getSimpleName());

        String failureMessage = t.getMessage(); 
        if (failureMessage == null && t.getCause() != null) {
            failureMessage = t.getCause().getMessage();
        }
        if (failureMessage != null) {
            builder.append('\n');
            builder.append(failureMessage);
        }

        // we skip all StackTraceElements that are targeting to Assert
        // to receive just the line, which called the Assert
        StackTraceElement[] failureStackTrace = t.getStackTrace();
        failureStackTrace = HelperMethods.getFilteredStackTrace(failureStackTrace);
        if (failureStackTrace.length > 0) {
            builder.append('\n');
            builder.append(failureStackTrace[0].toString());
        }

        return builder.toString();
    }

    @Override
    public boolean isComparisonFailure(Throwable throwable) {
        return this.delegateClassifiesThrowables.isComparisonFailure(throwable);
    }
}
