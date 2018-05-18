package org.codecover.eclipse.views;

import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.eclipse.utils.EclipseMASTLinkage.MAST;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.HierarchyLevel;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;

public abstract class CodeCoverView extends ViewPart {
	
	/**
     * Locks the update of the viewer and the field {@link #activeTSCInfo}.
     */
	protected Object updateLock = new Object();

	private static final String DIALOG_ERROR_NO_CODE_MSG = Messages.getString("CoverageView.DIALOG_ERROR_NO_CODE_MSG"); //$NON-NLS-1$

	private static final String DIALOG_ERROR_NO_CODE_TITLE = Messages.getString("CoverageView.DIALOG_ERROR_NO_CODE_TITLE"); //$NON-NLS-1$

	/**
     * <code>true</code>, if there is an update of {@link #activeTSCInfo} (and
     * thus an update of the viewer) pending (this also means that an active
     * test session container is queued in {@link #queuedActiveTSCInfo}),
     * <code>false</code> otherwise
     */
    protected boolean updatePending;
	
	 /**
     * Locks the update queue which only has a size of one and consists of
     * {@link #queuedActiveTSCInfo} and {@link #updatePending} which tells if
     * the queue is full or empty.
     */
    protected Object queueLock = new Object();
	
	/**
	 * The <code>ActiveTSContainerInfo</code> which contains the test session
	 * container which is visualized by this view and the active test cases
	 * which are visualized by this view.
	 */
	protected ActiveTSContainerInfo activeTSCInfo;

	/**
	 * The <code>ActiveTSContainerInfo</code> which is queued and will be used
	 * to update the viewer. This queuing mechanism reduces the number of
	 * updates if for example multiple test cases are deleted in a short period
	 * of time (this happens when deleting a test session).
	 */
	protected ActiveTSContainerInfo queuedActiveTSCInfo;

	/**
	 * Open and shown in Editor if possible. Inform user of errors.
	 * 
	 * @param hLev
	 *            a java element that has code (no package)
	 */
	protected void showHierarchyLevelInEditor(HierarchyLevel hLev) {
		TestSessionContainer tsc;
		tsc = CodeCoverView.this.getVisTSC();

		/* open hLev in a text editor with highlighting */
		ITextEditor editor = EclipseMASTLinkage.openClassInEditor(hLev, tsc); // FIXME
																				// Ralf.

		if (editor == null) {
			/* failed: no suitable Resource can be opened */

			String mesg = String.format(DIALOG_ERROR_NO_CODE_MSG, hLev.getName());
			MessageDialog.openWarning(CodeCoverView.this.getSite().getShell(), DIALOG_ERROR_NO_CODE_TITLE, mesg);
		} else {
			/* show hLev in the Editor */
			EclipseMASTLinkage.showInEditor(editor, MAST.getHighlightLocation(hLev));
		}
	}

	/**
	 * Returns the <code>TestSessionContainer</code> which is currently
	 * visualized by this view.
	 * 
	 * @return the <code>TestSessionContainer</code> which is currently
	 *         visualized by this view.
	 */
	protected TestSessionContainer getVisTSC() {
		synchronized (this.updateLock) {
			return (this.activeTSCInfo != null) ? this.activeTSCInfo.getTestSessionContainer() : null;
		}
	}

	/**
	 * Returns the <code>TSContainerInfo</code>-representation of the test
	 * session container which is currently visualized by this view.
	 * 
	 * @return the <code>TSContainerInfo</code>-representation of the test
	 *         session container which is currently visualized by this view.
	 */
	protected TSContainerInfo getVisTSCInfo() {
		synchronized (this.updateLock) {
			return (this.activeTSCInfo != null) ? this.activeTSCInfo.getTSContainerInfo() : null;
		}
	}
	
	protected void performUpdate() {
		boolean vorher = this.activeTSCInfo != null;
        synchronized(this.updateLock) {
            synchronized(this.queueLock) {
            	if (this.activeTSCInfo != null && this.queuedActiveTSCInfo == null) { 
            		System.out.println("AH ! Wird kaputtgemacht! Active -> queued -> null"); 
            		
            	}
                this.activeTSCInfo = this.queuedActiveTSCInfo;
                if (activeTSCInfo == null) {
            		System.out.println("AH! queued -> null");
            	}
                this.queuedActiveTSCInfo = null;
                this.updatePending = false;
            }

        }
        boolean nacher = this.activeTSCInfo != null;
        if (vorher && !nacher) {
        	System.out.println("performUpdate hat activeTSCInfo auf null gesetzt");
        }
    }
	
	
	/**
     * Sets the input of the viewer to the given data.
     */
    protected void setViewerInput(ActiveTSContainerInfo activeTSCInfo) { // generisch
        Display display = this.getViewSite().getShell().getDisplay();
        synchronized(this.queueLock) {
        	if (activeTSCInfo == null) {
        		System.out.println("AH! queued -> activeTSCInfo -> null");
        	}
            this.queuedActiveTSCInfo = activeTSCInfo;
            if(this.updatePending) {
                /*
                 * if there is already an update pending, don't queue another
                 * one
                 */
                return;
            } else {
                /*
                 * if there is no update pending, queue a runnable which
                 * performs the update
                 */
                this.updatePending = true;
            }
        }

        // queue an update of the viewer
        display.asyncExec(new Runnable() {
            @Override
			public void run() {
                CodeCoverView.this.performUpdate();
            }
        });
    }
	
	

}
