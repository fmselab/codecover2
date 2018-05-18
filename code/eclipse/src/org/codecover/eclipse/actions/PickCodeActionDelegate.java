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
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ComplexStatement;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.MetaDataObject;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.mast.SynchronizedStatement;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * <p>
 * Class to ease the creation of an Action that processes the MAST-Element
 * currently selected in the Editor.
 * </p>
 * <p>
 * Finds the MAST-Element that best matches the current selection of the editor
 * and dispatch the method {@link #runNotSearched(ITextEditor)},
 * {@link #runNotFound(ActiveTSContainerInfo, ITextEditor)} or
 * {@link #runFound(MetaDataObject, ActiveTSContainerInfo, ITextEditor)}. The
 * best match is defined {@link #getPickVisitor()}. It defaults to match the
 * element with the smallest location that completely contains the editors
 * Selection.
 * </p>
 * <p>
 * Clients overwrite {@link #runNotSearched(ITextEditor)},
 * {@link #runNotFound(ActiveTSContainerInfo, ITextEditor)} and
 * {@link #runFound(MetaDataObject, ActiveTSContainerInfo, ITextEditor)} to run
 * their specific Action. They overwrite {@link #getPickVisitor()} to return a
 * {@link PickVisitor} that returns the suitable element for them. See its
 * comments to know how to do this best and {@link PickTestCaseActionDelegate}
 * for an example.
 * 
 * @author Johannes Langauf
 * @version 1.0 ($Id: PickCodeActionDelegate.java 2248 2007-10-31 16:55:48Z
 *          wittlims $)
 */
public abstract class PickCodeActionDelegate implements
        IEditorActionDelegate {

    private static final String PICK_CODE_NOT_FOUND_WARNING_MESSAGE = Messages
            .getString("PickCodeActionDelegate.PICK_CODE_NOT_FOUND_WARNING_MESSAGE"); //$NON-NLS-1$

    private static final String PICK_CODE_NOT_FOUND_WARNING_TITLE = Messages
            .getString("PickCodeActionDelegate.PICK_CODE_NOT_FOUND_WARNING_TITLE"); //$NON-NLS-1$

    private static final String PICK_CODE_NOT_SEARCHED_WARNING_MESSAGE = Messages
            .getString("PickCodeActionDelegate.PICK_CODE_NOT_SEARCHED_WARNING_MESSAGE"); //$NON-NLS-1$

    private static final String PICK_CODE_NOT_SEARCHED_WARNING_TITLE = Messages.getString("PickCodeActionDelegate.PICK_CODE_NOT_SEARCHED_WARNING_TITLE"); //$NON-NLS-1$

    IEditorPart targetEditor = null;

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
     *      org.eclipse.ui.IEditorPart)
     */
    @Override
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
        this.targetEditor = targetEditor;
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    @Override
	public void run(IAction action) {
        if (targetEditor != null
                && targetEditor instanceof ITextEditor) {
            if (! runElementAction((ITextEditor) targetEditor)) {
                runNotSearched((ITextEditor) targetEditor);
            }
        }
    }

    /**
     * Find the MAST-Element that best matches the current selection of
     * <code>editor</code> and dispatch the method {@link #runNotSearched()},
     * {@link #runNotFound()} or {@link #runFound()}.
     * 
     * @param editor
     * @return false if action failed before searching the MAST
     */
    private boolean runElementAction(ITextEditor editor) {
        CodeCoverPlugin plugin = CodeCoverPlugin.getDefault();
        ActiveTSContainerInfo activeTSCInfo =
            plugin.getTSContainerManager().getActiveTSContainer();
        
        if (activeTSCInfo == null) {
            /* no tsc active */
            return false;
        }
        TestSessionContainer tsc = activeTSCInfo.getTestSessionContainer();
        
        SourceFile file = getSourceFile(editor, tsc);
        if (file == null) {
            /* found no matching source file */
            return false;
        }
        Location selection = EclipseMASTLinkage.getSelection(editor,
                file);
        
        /* pickedLocation represents the selection of editor */

        MetaDataObject pickedElement = findSurroundingElement(selection, tsc);
        if (pickedElement == null) {
            runNotFound(activeTSCInfo, editor);
        } else {
            runFound(pickedElement, activeTSCInfo, editor);
        }
        return true;
    }

    /**
     * Find the MAST-Element with the smallest Location that containins key.
     * If there is more than one chose an element that is not parent of another
     * match with the same length.
     *  
     * @param tsc
     * the test session container to search in 
     * @param key
     * a location, that is part of the best match
     * @return
     * the best match, null for none
     */
    //FIXME: this is not declared static, because java sucks
    public MetaDataObject findSurroundingElement(Location key,
            TestSessionContainer tsc) {
        return getPickVisitor().findElement(tsc, key);
    }
    
    /**
     * @return a visitor that picks suitable MAST-Elements for this action
     */
    //FIXME: this is static, but couldn't be overwritten ==> java sucks
    protected PickVisitor getPickVisitor() {
        return new PickVisitor();
    }
    
    /**
     * Called when no MAST element is found
     * 
     * @param editor
     *            the given editor.
     */
    protected void runNotSearched(ITextEditor editor) {
        MessageDialog.openWarning(editor.getSite().getShell(),
                PICK_CODE_NOT_SEARCHED_WARNING_TITLE, PICK_CODE_NOT_SEARCHED_WARNING_MESSAGE);
    }

    /**
     * Called when no MAST element is found.
     * 
     * @param activeTSCInfo
     *            FIXME is not used, remove?
     * @param editor
     *            the given editor.
     */
    protected void runNotFound(ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
        MessageDialog.openWarning(editor.getSite().getShell(),
                PICK_CODE_NOT_FOUND_WARNING_TITLE, PICK_CODE_NOT_FOUND_WARNING_MESSAGE);
    }
    
    /**
     * Called when the MAST element was found.
     * 
     * @param pickedElement
     *            not null
     * @param activeTSCInfo
     *            not null
     * @param editor
     *            not null
     */
    protected void runFound(MetaDataObject pickedElement,
            ActiveTSContainerInfo activeTSCInfo, ITextEditor editor) {
    }
    
    /**
     * Traverses a given test session container to find a MAST element that
     * matches a given Location best.<p>
     * To choose which elements to pick overwrite the visit() methods to first
     * call super() and then updateMatch() to update the best match with its
     * locations. Per default no element is chosen. Also overwrite
     * {@link #traversePostfix(TestSessionContainer)} to deeper levels you are
     * not interested in. Per default everything is traversed.
     * 
     */
    public static class PickVisitor implements Statement.Visitor,
            RootTerm.Visitor, BooleanTerm.Visitor, HierarchyLevel.Visitor {

        /**
         * the search key (alias inner location of what we are searching)
         */
        private Location key;

        protected int bestMatchLength;

        protected Location bestMatch;
        
        //the picked MAST element
        protected MetaDataObject bestMatchElement;
        
        /**
         * Constructor.
         */
        protected PickVisitor() {
        }

        /**
         * 
         *  
         * @param tsc
         * the test session container to search in 
         * @param key
         * a location, that is part of the best match
         * @return
         * the best match, null for none
         */
        public MetaDataObject findElement(TestSessionContainer tsc, Location key) {
            this.key = key;
            
            bestMatch = null;
            bestMatchElement = null;
            bestMatchLength = -1;
            
            traversePostfix(tsc);
            
            return bestMatchElement;
        }

        /**
         * Traverse MAST in postfix order to prefer matches deeper down the
         * tree.
         * 
         * @param tsc
         *            the {@link TestSessionContainer} holding the MAST.
         */
        protected void traversePostfix(TestSessionContainer tsc) {
            tsc.getCode().accept(null, this, null, this, null, this, null, this, null);
            //ignore what you don't need: i.g. to ignore BooleanTerms:
            //tsc.getCode().accept(null, this, null, this, null, this, null, null);
        }
        
        /**
         * Gets the best match.
         * 
         * @return the best match.
         */
        public Location getBestMatch() {
            return bestMatch;
        }
        
        /**
         * Gets the best matching MAST-node.
         * 
         * @return the best matching node.
         */
        public MetaDataObject getBestNode() {
            return bestMatchElement;
        }
        
        
        /*
         * Type specific updateMatch, considering all of elements locations.
         */
        
        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(ComplexStatement element) {
            Location[] ls = {element.getKeyword()};
            LocationList[] lls = {element.getLocation()};
            updateMatch(ls, lls, element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(RootTerm element) {
            updateMatch(element.getTerm().getLocation(), element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(Branch element) {
            LocationList[] lls = {element.getDecision(), element.getLocation()};
            updateMatch(lls, element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(BasicStatement element) {
            updateMatch(element.getLocation(), element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(StatementSequence element) {
            updateMatch(element.getLocation(), element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(BooleanTerm element) {
            updateMatch(element.getLocation(), element);
        }

        /**
         * Update current best match with <code>element</code>. It is only
         * taken if it matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         */
        protected void updateMatch(HierarchyLevel element) {
            LocationList[] lls = {element.getHeader(), element.getLocation()};
            updateMatch(lls, element);
        }
             
        /*
         * Generic updateMatch methods.
         */
        
        /**
         * Update current best match with this element. It is only taken if it
         * matches and is better than the current best match. Means that
         * <code>locs</code> form a single logical location.
         * 
         * @param element
         *            the given element.
         * @param locs
         *            the locations to consider for this element.
         * @param locLists
         *            the location lists to consider for this element.
         * @return FIXME what exactly is returned?
         */
        protected boolean updateMatch(Location[] locs, LocationList[] locLists,
                MetaDataObject element) {
            /* This Method may be used to implement more intelligent matching
             * of many Locations. Currently makes no difference.
             */
            
            boolean retval = false;
            
            for (Location l: locs) {
                if (updateMatch(l, element)) {
                    retval = true;
                }
            }
            for (LocationList l : locLists) {
                if (updateMatch(l, element)) {
                    retval = true;
                }
            }
            
            return retval;
        }
        
        /**
         * {@link PickVisitor#updateMatch(Location[], LocationList[], MetaDataObject)}
         * with empty locs.
         * 
         * @param locLists
         *            the {@link LocationList} to consider
         * @param element
         *            the element to consider.
         * @return FIXME what exactly is returned?
         * @see PickVisitor#updateMatch(Location[], LocationList[],
         *      MetaDataObject)
         */
        protected boolean updateMatch(LocationList[] locLists, MetaDataObject element) {
            final Location[] noLocation = {};
            return updateMatch(noLocation, locLists, element);
        }
       
        /**
         * Update current best match with this element. It is only taken if it
         * matches and is better than the current best match.
         * 
         * @param element
         *            the given element.
         * @param locs
         *            the locations to consider for this element.
         * @return FIXME what exactly is returned?
         */
        protected boolean updateMatch(LocationList locs, MetaDataObject element) {
            boolean retval = false;
            
            for (Location l : locs.getLocations()) {
                if (updateMatch(l, element)) {
                    retval = true;
                }
            }
            
            return retval;
        }
        
        /**
         * Update current best match with this element. It is only taken if it
         * matches and is better than the current best match.
         * 
         * @param element
         *            the given element
         * @param l
         *            the location to consider for <code>element</code>
         * @return FIXME what exactly is returned?
         */
        protected boolean updateMatch(Location l, MetaDataObject element) {
            if (l.contains(key)) {
                if (bestMatchLength == -1
                        || l.getEndOffset() - l.getStartOffset() < bestMatchLength) {
                    /* l is better because we have nothing or it is smaller */
                    updateBestMatch(l, element);
                    return true;
                } else {
                    /* l contains key but is a worse match */
                }
            }
            return false;
        }
        
        /**
         * Update fields for new best match. Called whenever a better match is
         * found.
         * 
         * @param element
         *            the given element
         * @param l
         *            the matching location of <code>element</code>
         */
        protected void updateBestMatch(Location l, MetaDataObject element) {
            bestMatchLength = l.getEndOffset() - l.getStartOffset();
            bestMatch = l;
            bestMatchElement = element;
        }
        
        /*
         * run update match functions on relevant locations to find best match
         */
        @Override
		public void visit(RootTerm term) {
        }

        @Override
		public void visit(Branch branch) {
        }

        @Override
		public void visit(BasicStatement statement) {
        }

        @Override
		public void visit(ConditionalStatement statement) {
        }
        
        @Override
		public void visit(LoopingStatement statement) {
        }

        @Override
		public void visit(StatementSequence sequence) {
        }

        @Override
		public void visit(BasicBooleanTerm term) {
        }

        @Override
		public void visit(OperatorTerm term) {
        }

        @Override
		public void visit(HierarchyLevel hierarchyLevel) {
        }

		@Override
		public void visit(SynchronizedStatement statement) {
		}
    }

    /**
     * Gets the {@link SourceFile} opened in the given editor from the given
     * {@link TestSessionContainer}
     * 
     * @param editor
     *            the given {@link ITextEditor}
     * @param tsc
     *            the given {@link TestSessionContainer}
     * @return the {@link SourceFile} or <code>null</code>, if no such
     *         {@link SourceFile} could be found.
     */
    public static SourceFile getSourceFile(ITextEditor editor, TestSessionContainer tsc) {
        /* get element Java-Element from within editor */
        IEditorInput input = editor.getEditorInput();
        if (input == null) {
            return null;
        }
        Object element = input.getAdapter(IJavaElement.class);
        if (element == null) {
            return null;
        }
        
        /* get corresponding hierarchy level */
        HierarchyLevel hLvl = getHierarchyLevel(editor, tsc);
        if (hLvl == null){
            return null;
        }
        
        /* Get any source file from this hierarchy level inside the editor */
        try {
            //XXX: handle HierarchyLevel in more than one Source file better
            return hLvl.getLocation().getLocations().get(0).getFile();
        } catch (Exception e) {
            CodeCoverPlugin.getDefault().getLogger().warning(
                    "Failed to get source file from HierarchyLevel", e); //$NON-NLS-1$
        }
        
        return null;
    }
    
    /**
     * Find a HierarchyLevel that is contained in <code>tsc</code> and overlaps
     * with the code shown in <code>editor</code>. 
     * 
     * @param editor
     * @param tsc
     * @return the search result, null if none found
     */
    public static HierarchyLevel getHierarchyLevel(ITextEditor editor,
            TestSessionContainer tsc) {
        //XXX: only works with Java
        /* get element Java-Element from within editor */
        IEditorInput input = editor.getEditorInput();
        if (input == null) {
            return null;
        }
        Object element = input.getAdapter(IJavaElement.class);
        if (element == null) {
            return null;
        }
        
        /* get corresponding hierarchy level */
        return EclipseMASTLinkage.findSource(tsc.getCode(),
                (IJavaElement) element);
    }
    
    @Override
	public void selectionChanged(IAction action, ISelection selection) {
        // ignore
    }

}
