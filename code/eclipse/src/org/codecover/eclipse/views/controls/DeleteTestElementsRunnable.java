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

package org.codecover.eclipse.views.controls;

import java.lang.reflect.InvocationTargetException;

import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.AbstractActiveTSContainerRunnable;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * A <code>ActiveTSContainerRunnable</code> which deletes the given test
 * elements of the active test session container. The runnable throws a
 * (wrapped) <code>ConcurrentModificationException</code> during execution if an
 * other test session container was selected during the user prepared the
 * deletion of test elements (e.g., during the user selected the test elements
 * to delete from a dialog).
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: DeleteTestElementsRunnable.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DeleteTestElementsRunnable
        extends AbstractActiveTSContainerRunnable {
    
    private static final String DESCRIPTION = Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_DESCRIPTION"); //$NON-NLS-1$
    
    final Object[] elementsToDelete;
    
    /**
     * Constructs a <code>ActiveTSContainerRunnable</code> which deletes the
     * given test elements of the active test session container.
     * 
     * @param elementsToDelete  the test elements to delete
     * 
     * @throws NullPointerException if the given test elements don't belong to
     *                              the same <code>TestSessionContainer</code>
     *                              or if the given test array is empty
     */
    public DeleteTestElementsRunnable(Object[] elementsToDelete)
            throws NullPointerException {
        super(fetchParentTSC(elementsToDelete));
        this.elementsToDelete = elementsToDelete;
    }
    
    /* (non-Javadoc)
     * @see org.codecover.eclipse.tscmanager.ActiveTSContainerRunnable#getDescription()
     */
    @Override
    public String getDescription() {
        return DESCRIPTION;
    }
    
    /* (non-Javadoc)
     * @see org.codecover.eclipse.tscmanager.ActiveTSContainerRunnable#run(org.codecover.eclipse.tscmanager.ActiveTSContainerInfo, org.eclipse.core.runtime.IProgressMonitor)
     */
    @Override
    public void runTask(ActiveTSContainerInfo activeTSCInfo,
                    IProgressMonitor monitor)
            throws InvocationTargetException, CancelException {
        // this try only ensure that the monitor is left done()
        try { 
            monitor.beginTask(DESCRIPTION, this.elementsToDelete.length);
            for(Object element : this.elementsToDelete) {
                if(element instanceof TestSession) {
                    ((TestSession)element).delete();
                } else if(element instanceof TestCase) {
                    ((TestCase)element).delete();
                }
                monitor.worked(1);
            }
        } finally {
            monitor.done();
        }
    }

    /**
     * Returns the parent test session container of the given test elements.
     * 
     * @param elements  the test elements
     * 
     * @return  the parent test session container of the given test elements or
     *          <code>null</code> if the given test elements don't have the same
     *          parent test session container or if the given array is empty
     */
    private static TestSessionContainer fetchParentTSC(Object[] elements) {
        TestSessionContainer parentTSC = null;
        TestSessionContainer currentParentTSC = null;
        for(Object element : elements) {
            if(element instanceof TestCase) {
                currentParentTSC = ((TestCase)element).getTestSession()
                        .getTestSessionContainer();
            } else if(element instanceof TestSession) {
                currentParentTSC = ((TestSession)element)
                        .getTestSessionContainer();
            }
            if(parentTSC != null) {
                if(currentParentTSC != parentTSC) {
                    return null;
                }
            } else {
                parentTSC = currentParentTSC;
            }
        }
        return parentTSC;
    }

}
