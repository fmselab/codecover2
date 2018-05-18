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

package org.codecover.eclipse.views;

import org.codecover.eclipse.*;
import org.codecover.eclipse.utils.ImageProvider;
import org.eclipse.jdt.core.*;
import org.eclipse.jface.viewers.*;

/**
 * This decorator is used to decorate an instrumentable item (package, file) as
 * being <em>used for coverage measurement<em>.
 *
 * @author Robert Hanussek, Markus Wittlinger
 * @version 1.0 ($Id: UseForCoverageMeasurementDecorator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UseForCoverageMeasurementDecorator implements
        ILightweightLabelDecorator {

    /**
     * The id of the decorator
     */
    public static final String ID = "org.codecover.eclipse.useForCoverageMeasurementDecorator"; //$NON-NLS-1$

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.ILightweightLabelDecorator#decorate(java.lang.Object,
     *      org.eclipse.jface.viewers.IDecoration)
     */
    @Override
	public void decorate(Object element, IDecoration decoration) {
        CodeCoverPlugin codeCoverPlugin = CodeCoverPlugin.getDefault();
        if (codeCoverPlugin == null) {
            return;
        }

        InstrumentableItemsManager instrumentableItemsManager = codeCoverPlugin
                .getInstrumentableItemsManager();
        if (instrumentableItemsManager == null) {
            return;
        }

        if (element instanceof IPackageFragment) {
            IPackageFragment packageFragment = (IPackageFragment) element;

            if (instrumentableItemsManager
                    .hasIPackageFragmentSubEntries(packageFragment)) {
                decoration.addOverlay(ImageProvider.getDecorator());
            }
        } else if (element instanceof ICompilationUnit) {
            ICompilationUnit compilationUnit = (ICompilationUnit) element;

            if (instrumentableItemsManager
                    .containsICompilationUnit(compilationUnit)) {
                decoration.addOverlay(ImageProvider.getDecorator());
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    @Override
	public void addListener(ILabelProviderListener listener) {
        // We don't do anything here.
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
     */
    @Override
	public void dispose() {
        // We don't do anything here.
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
     *      java.lang.String)
     */
    @Override
	public boolean isLabelProperty(Object element, String property) {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
     */
    @Override
	public void removeListener(ILabelProviderListener listener) {
        // We don't do anything here.
    }
}
