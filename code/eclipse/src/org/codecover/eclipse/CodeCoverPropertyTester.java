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

package org.codecover.eclipse;

import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.internal.junit.JUnitPropertyTester;

/**
 * Tests an object, if its project is activated for CodeCover.
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: CodeCoverPropertyTester.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CodeCoverPropertyTester extends PropertyTester {

    /**
     * A property for checking, if an launchable object is in a project, which
     * is activated for CodeCover.
     */
    public static final String PROPERTY_IS_CODECOVER_ACTIVATED = "isCodeCoverActivated"; //$NON-NLS-1$
    
    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object,
     *      java.lang.String, java.lang.Object[], java.lang.Object)
     */
    @Override
	public boolean test(Object receiver,
                        String property,
                        Object[] args,
                        Object expectedValue) {
        if (!(receiver instanceof IAdaptable)) {
            throw new IllegalArgumentException("Element must be of type 'IAdaptable', " + //$NON-NLS-1$
                    "is " + receiver == null ? "null" : receiver.getClass().getName()); //$NON-NLS-1$ //$NON-NLS-2$
        }

        IJavaElement javaElement = convertToJavaElement(receiver);
        return test(javaElement, property, args, expectedValue);
    }

    /**
     * Tries to convert <code>objectToCheck</code> to an {@link IJavaElement}.
     * 
     * Copied from {@link JUnitPropertyTester#test(Object, String, Object[], Object)}.
     * 
     * @param objectToConvert The object to convert.
     * @return <code>null</code> &rarr; conversation was not possible; else
     * &rarr; object converted to an {@link IJavaElement}.
     */
    protected IJavaElement convertToJavaElement(Object objectToConvert) {
        IJavaElement element = null;
        if (objectToConvert instanceof IJavaElement) {
            element = (IJavaElement) objectToConvert;
        } else if (objectToConvert instanceof IResource) {
            element = JavaCore.create((IResource) objectToConvert);
            if (element == null) {
                return null;
            }
        } else { // is IAdaptable
            element = ((IAdaptable) objectToConvert)
                    .getAdapter(IJavaElement.class);
            if (element == null) {
                IResource resource = ((IAdaptable) objectToConvert)
                    .getAdapter(IResource.class);
                element = JavaCore.create(resource);
                if (element == null) {
                    return null;
                }
                return null;
            }
        }
        return element;
    }

    private boolean test(IJavaElement receiver,
                           String property,
                           Object[] args,
                           Object expectedValue) {
        if (receiver == null) {
            return false;
        }

        if (property.equals(PROPERTY_IS_CODECOVER_ACTIVATED)) {
            return isCodeCoverActivated(receiver, property, args, expectedValue);
        } else {
            // unknown property
            return false;
        }
    }

    private boolean isCodeCoverActivated(IJavaElement receiver,
                                       String property,
                                       Object[] args,
                                       Object expectedValue) {
        IJavaProject javaProject = receiver.getJavaProject();
        if (javaProject == null) {
            return false;
        } else {
            return CodeCoverPlugin.isCodeCoverActivated(javaProject.getProject());
        }
    }
}
