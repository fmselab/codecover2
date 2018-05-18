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

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.views.BooleanAnalyzer;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Display boolean analyzer for the root term which is currently selected in
 * the text editor. 
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: AnalyzeRootTermActionDelegate.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class AnalyzeRootTermActionDelegate extends
        PickCodeActionDelegate {

    private static final String ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_MESSAGE = Messages
            .getString("AnalyzeRootTermActionDelegate.ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_MESSAGE"); //$NON-NLS-1$

    private static final String ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_TITLE = Messages
            .getString("AnalyzeRootTermActionDelegate.ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_TITLE"); //$NON-NLS-1$

    /**
     * @return a visitor that picks suitable MAST-Elements for this action
     */
    @Override
    protected PickVisitor getPickVisitor() {
        return new PickRootTermVisitor();
    }

    /**
     * Called when no MAST element is found
     */
    @Override
    protected void runNotFound(ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
        MessageDialog.openWarning(editor.getSite().getShell(),
                ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_TITLE,
                ANALYZE_ROOT_TERM_NOT_FOUND_WARNING_MESSAGE);
    }
    
    @Override
    protected void runFound(MetaDataObject picked,
            ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
        if (! (picked instanceof RootTerm)) {
           throw new IllegalArgumentException("picked element is no RootTerm:" //$NON-NLS-1$
                   + picked.getClass().getCanonicalName()); 
        }
        RootTerm rootTerm = (RootTerm) picked;
        
        TestSessionContainer tsc = activeTSCInfo.getTestSessionContainer();
        showInBooleanAnalyzer(rootTerm, getHierarchyLevel(editor, tsc));
    }
    
    private void showInBooleanAnalyzer(final RootTerm term,
            final HierarchyLevel hLvl) {
        showBooleanAnalyzer();
        
        Display.getDefault().asyncExec(new Runnable() {
            @Override
			public void run() {
                displayTerm(term, hLvl);
            }
        });
    }

    private void displayTerm(RootTerm term, HierarchyLevel hLvl) {
        final CodeCoverPlugin plugin = CodeCoverPlugin.getDefault();
        BooleanAnalyzer ba = plugin.getBooleanAnalyzer();
        /* wait for boolean analyzer, timeout 10s */

        if (ba == null) {
            CodeCoverPlugin.getDefault().getLogger().error(
            "Failed to open boolean analyzer for picked root term: ba == null"); //$NON-NLS-1$
        } else {
            try {
                ba.displayRootTerm(hLvl, term);
            } catch (RuntimeException e) {
                CodeCoverPlugin.getDefault().getLogger().error("gave up to show term:", e); //$NON-NLS-1$
            }
        }
    }

    private void showBooleanAnalyzer() {
        Display.getDefault().syncExec(new Runnable() {
            @Override
			public void run() {
                IWorkbenchWindow activeWindow;
                IWorkbenchPage activePage;
                activeWindow = PlatformUI.getWorkbench().getActiveWorkbenchWindow();

                if (activeWindow != null) {
                    activePage = activeWindow.getActivePage();
                    if (activePage != null) {
                        try {
                            activePage.showView(
                                "org.codecover.eclipse.views.BooleanAnalyzer"); //$NON-NLS-1$

                        } catch(PartInitException e) {
                            CodeCoverPlugin.getDefault().getLogger().error(
                                    "Boolean Analyzer view couldn't" + //$NON-NLS-1$
                                    " be created.", e);                //$NON-NLS-1$
                        }
                    }
                }
            }
        });
    }
    
    /**
     * Traverses a given test session container to find a RootTerm from its
     * location.
     */
    private static class PickRootTermVisitor extends
            PickCodeActionDelegate.PickVisitor {

        @Override
        protected void traversePostfix(TestSessionContainer tsc) {
            //ignore HierarchyLevels and BooleanTerms
            tsc.getCode().accept(null, null, null, null, null, this, null, null, null);
        }

        /*
         * run update match functions on relevant locations to find best match
         */
        @Override
        public void visit(RootTerm term) {
            super.visit(term);
            updateMatch(term);
            
            //XXX: BEGIN workaround for BUG 
            // picked a BasicBooleanTerm or OperatorTerm:
            //   make sure match is RootTerm
            if (! (getBestNode() instanceof RootTerm)) {
                this.bestMatchElement = term;
            }
            //END workaround
        }
        
        //BEGIN workaround
        @Override
		public void visit(BasicBooleanTerm term) {
            super.visit(term);
            updateMatch(term);
        }

        @Override
		public void visit(OperatorTerm term) {
            super.visit(term);
            updateMatch(term);
        }
        //END workaround
    }
}
