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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.InstrumentableItemsManager;
import org.codecover.eclipse.views.UseForCoverageMeasurementDecorator;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;

/**
 * This action is used to mark an instrumentable item as <em>normal</em> or
 * <em>used for coverage measurement<em>. It is part of the context menu of the
 * Package Explorer.
 *
 * @author Robert Hanussek, Markus Wittlinger
 * @version 1.0 ($Id: UseForCoverageMeasurementActionDelegate.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UseForCoverageMeasurementActionDelegate implements
        IObjectActionDelegate {

    /**
     * The selection which contains the elements this action can affect.
     */
    private ISelection selection;

    private InstrumentableItemsManager instrumentableItemsManager;

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    @Override
	public void run(IAction action) {
        if (!(this.selection instanceof IStructuredSelection)) {
            return;
        }

        Set<IProject> projects = new HashSet<IProject>();

        for (Iterator<?> i = ((IStructuredSelection) this.selection).iterator(); i
                .hasNext();) {
            Object o = i.next();

            if (o instanceof IPackageFragment) {
                IPackageFragment element = (IPackageFragment) o;

                // Get all the subelements of this package
                for (Map.Entry<IPackageFragment, Set<ICompilationUnit>> entry : getChildren(
                        element).entrySet()) {
                    for (ICompilationUnit compilationUnit : entry.getValue()) {
                        if (action.isChecked()) {
                            getContainerManager().addICompilationUnit(
                                    compilationUnit);
                        } else {
                            getContainerManager().removeICompilationUnit(
                                    compilationUnit);
                        }
                    }
                }

                projects.add(element.getResource().getProject());
            } else if (o instanceof ICompilationUnit) {
                ICompilationUnit element = (ICompilationUnit) o;
                if (element.getParent() instanceof IPackageFragment) {
                    if (action.isChecked()) {
                        getContainerManager().addICompilationUnit(element);
                    } else {
                        getContainerManager().removeICompilationUnit(element);
                    }
                }

                projects.add(element.getResource().getProject());
            }
        }

        // Update the UseForCoverageMeasurementDecorator
        PlatformUI.getWorkbench().getDecoratorManager().update(
                UseForCoverageMeasurementDecorator.ID);

        // check which projects should be rebuilt
        for (IProject project : projects) {
            CodeCoverPlugin.build(project);
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
     *      org.eclipse.jface.viewers.ISelection)
     */
    @Override
	public void selectionChanged(IAction action, ISelection selection) {
        this.selection = selection;

        if (!(selection instanceof StructuredSelection)) {
            return;
        }

        IStructuredSelection structuredSelection = (IStructuredSelection) selection;

        boolean checkedState = false;
        boolean enabledState = true;

        for (Iterator<?> i = structuredSelection.iterator(); i.hasNext();) {
            Object o = i.next();
            if (o instanceof IPackageFragment) {
                IPackageFragment packageFragment = (IPackageFragment) o;
                try {
                    if (!packageFragment.hasChildren()) {
                        enabledState = false;
                    }
                } catch (JavaModelException e) {
                    CodeCoverPlugin.getDefault().getLogger().fatal(
                            "A JavaModelException occured", e); //$NON-NLS-1$
                }

                checkedState |= getContainerManager()
                        .hasIPackageFragmentSubEntries(packageFragment);

            } else if (o instanceof ICompilationUnit) {
                ICompilationUnit compilationUnit = (ICompilationUnit) o;

                checkedState |= getContainerManager().containsICompilationUnit(
                        compilationUnit);
            } else {
                enabledState = false;
            }
        }

        // prevents the action being applied to empty selections
        enabledState &= !structuredSelection.isEmpty();

        action.setChecked(checkedState);
        action.setEnabled(enabledState);
    }

    private Map<IPackageFragment, Set<ICompilationUnit>> getChildren(
            IPackageFragment element) {
        Map<IPackageFragment, Set<ICompilationUnit>> elements = new HashMap<IPackageFragment, Set<ICompilationUnit>>();
        Set<IPackageFragment> packageFragments = new HashSet<IPackageFragment>();

        packageFragments.add(element);
        final String packageName = element.getElementName();

        try {
            if (element.getParent() instanceof IPackageFragmentRoot) {
                IPackageFragmentRoot parent = (IPackageFragmentRoot) element
                        .getParent();

                for (IJavaElement javaElement : parent.getChildren()) {

                    if (javaElement instanceof IPackageFragment) {
                        IPackageFragment packageFragment = (IPackageFragment) javaElement;
                        String fragmentName = packageFragment.getElementName();

                        // If the package starts with the same string as the
                        // given package, it is a subpackage of the given
                        // package
                        if (fragmentName.startsWith(packageName)) {
                            packageFragments.add(packageFragment);
                        }
                    }
                }
            }

            for (IPackageFragment packageFragment : packageFragments) {
                Set<ICompilationUnit> compilationUnits = new HashSet<ICompilationUnit>();
                elements.put(packageFragment, compilationUnits);

                for (ICompilationUnit compilationUnit : packageFragment
                        .getCompilationUnits()) {
                    compilationUnits.add(compilationUnit);
                }
            }
        } catch (JavaModelException e) {
            CodeCoverPlugin.getDefault().getLogger().fatal(
                    "A JavaModelException occured", e); //$NON-NLS-1$
        }

        return elements;
    }

    private InstrumentableItemsManager getContainerManager() {
        if (this.instrumentableItemsManager == null) {
            CodeCoverPlugin codeCoverPlugin = CodeCoverPlugin.getDefault();
            if (codeCoverPlugin == null) {
                throw new NullPointerException("codeCoverPlugin == null"); //$NON-NLS-1$
            }

            this.instrumentableItemsManager = codeCoverPlugin
                    .getInstrumentableItemsManager();
        }
        return this.instrumentableItemsManager;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
     *      org.eclipse.ui.IWorkbenchPart)
     */
    @Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
        // Nothing needs to be done here.
    }
}
