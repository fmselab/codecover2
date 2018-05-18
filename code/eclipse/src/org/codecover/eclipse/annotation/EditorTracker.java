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

/*******************************************************************************
 * This class is mostly identical to the EditorTracker from EclEmma.
 * Only package names, comments and white space was changed.
 * 
 * Original Copyright Information:
 * Copyright (c) 2006 Mountainminds GmbH & Co. KG
 * Author: Marc R. Hoffmann
 * Revision: 12
 ******************************************************************************/
package org.codecover.eclipse.annotation;

import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWindowListener;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * Tracks the workbench editors and attaches coverage annotation models where
 * appropriate.
 * 
 * @author  Johannes Langauf
 * @version 1.0 ($Id: EditorTracker.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EditorTracker {

    private final IWorkbench workbench;

    private IWindowListener windowListener = new IWindowListener() {
        @Override
		public void windowOpened(IWorkbenchWindow window) {
            window.getPartService().addPartListener(partListener);
        }
        @Override
		public void windowClosed(IWorkbenchWindow window) {
            window.getPartService().removePartListener(partListener);
        }
        @Override
		public void windowActivated(IWorkbenchWindow window) { }
        @Override
		public void windowDeactivated(IWorkbenchWindow window) { }
    };

    private IPartListener2 partListener = new IPartListener2() {
        @Override
		public void partOpened(IWorkbenchPartReference partref) { 
            annotateEditor(partref);
        }
        @Override
		public void partActivated(IWorkbenchPartReference partref) { }
        @Override
		public void partBroughtToTop(IWorkbenchPartReference partref) { }
        @Override
		public void partVisible(IWorkbenchPartReference partref) { }
        @Override
		public void partInputChanged(IWorkbenchPartReference partref) { }
        @Override
		public void partClosed(IWorkbenchPartReference partref) { }
        @Override
		public void partDeactivated(IWorkbenchPartReference partref) { }
        @Override
		public void partHidden(IWorkbenchPartReference partref) { }
    };

    /**
     * Constructor.
     * 
     * @param workbench
     *            the {@link IWorkbench} holding the editors to track.
     */
    public EditorTracker(IWorkbench workbench) {
        this.workbench = workbench;
        
        //note: has once thrown an NPE from getWorkbenchWindows(), during
        //  plugin initialization. Report Bug to Eclipse if you reproduce it.
        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
        
        for (int i = 0; i < windows.length; i++) {
            windows[i].getPartService().addPartListener(partListener);
        }
        workbench.addWindowListener(windowListener);
        annotateAllEditors();
    }

    /**
     * Removes all listeners, this tracker registered.
     */
    public void dispose() {
        workbench.removeWindowListener(windowListener);
        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
        for (int i = 0; i < windows.length; i++) {
            windows[i].getPartService().removePartListener(partListener);
        }
    }

    private void annotateAllEditors() {
        IWorkbenchWindow[] windows = workbench.getWorkbenchWindows();
        for (int i = 0; i < windows.length; i++) {
            IWorkbenchPage[] pages = windows[i].getPages();
            for (int j = 0; j < pages.length; j++) {
                IEditorReference[] editors = pages[j].getEditorReferences();
                for (int k = 0; k < editors.length; k++) {
                    annotateEditor(editors[k]);
                }
            }      
        }
    }

    private static void annotateEditor(IWorkbenchPartReference partref) {
        IWorkbenchPart part = partref.getPart(false);
        if (part instanceof ITextEditor) {
            CoverageAnnotationModel.attach((ITextEditor) part);
        }
    }
}
