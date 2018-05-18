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
 * Heavily modified, based on CoverageAnnoationModel from EclEmma:
 * Copyright (c) 2006 Mountainminds GmbH & Co. KG
 * Author: Marc R. Hoffmann
 * Revision: 12
 ******************************************************************************/
package org.codecover.eclipse.annotation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.annotation.hotpath.EclLineExecutionAnnotation;
import org.codecover.eclipse.tscmanager.*;
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.*;
import org.codecover.model.utils.ChangeType;
import org.codecover.report.highlighting.CodeHighlighting;
import org.codecover.report.highlighting.HighlightedSnippet;
import org.codecover.report.highlighting.annotation.*;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jface.text.*;
import org.eclipse.jface.text.source.AnnotationModelEvent;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.text.source.IAnnotationModelListener;
import org.eclipse.jface.text.source.IAnnotationModelListenerExtension;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;

/**
 * IAnnotationModel implementation for efficient coverage highlighting.
 * 
 * @author  Johannes Langauf
 * @version 1.0 ($Id: CoverageAnnotationModel.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CoverageAnnotationModel implements IAnnotationModel {

    /** Key used to piggyback our model to the editor's model. */
    private static final Object KEY = new Object();

    /** List of current Annotation objects */
    //private List<EclPositionedAnnotation> annotations = new ArrayList<EclPositionedAnnotation>(32);
    private List<org.eclipse.jface.text.source.Annotation> annotations = new ArrayList<org.eclipse.jface.text.source.Annotation>(32);

    /** List of registered IAnnotationModelListener */
    private List<IAnnotationModelListener> annotationModelListeners = new ArrayList<IAnnotationModelListener>(2);

    /**
     * A node containing all coverable items for the highlighted file.
     */
    private HierarchyLevel codeFile = null;
    private SourceFile highlightedFile = null;

    private final ITextEditor editor;
    private final IDocument document;
    private int openConnections = 0;
    private boolean annotated = false;

    private IDocumentListener documentListener = new IDocumentListener() {
        @Override
		public void documentChanged(DocumentEvent event) {
            updateAnnotations(false);
        }
        @Override
		public void documentAboutToBeChanged(DocumentEvent event) {
            // Ignore
        }
    };

    private TSManagerListener modelListener = new TSManagerListener();

    private CoverageAnnotationModel(ITextEditor editor, IDocument document) {
        this.editor = editor;
        this.document = document;
        updateAnnotations(true);
    }

    /**
     * Attaches a coverage annotation model for the given editor if the editor
     * can be annotated. Does nothing if the model is already attached.
     * 
     * @param editor Editor to attach an annotation model to
     */
    public static void attach(ITextEditor editor) {
        IAnnotationModel model = editor.getDocumentProvider().getAnnotationModel(editor.getEditorInput());

        if (!(model instanceof IAnnotationModelExtension)) {
            return;
        }
        IAnnotationModelExtension modelex = (IAnnotationModelExtension) model;

        IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());

        CoverageAnnotationModel coveragemodel = (CoverageAnnotationModel) modelex.getAnnotationModel(KEY);
        if (coveragemodel == null) {
            coveragemodel = new CoverageAnnotationModel(editor, document);
            modelex.addAnnotationModel(KEY, coveragemodel);
        }
    }

    /**
     * Detaches the coverage annotation model from the given editor. If the editor
     * does not have a model attached, this method does nothing.
     *
     * @param editor Editor to detach the annotation model from
     */
    public static void detach(ITextEditor editor) {
        IAnnotationModel model = editor.getDocumentProvider().getAnnotationModel(editor.getEditorInput());
        if (!(model instanceof IAnnotationModelExtension)) return;
        IAnnotationModelExtension modelex = (IAnnotationModelExtension) model;
        modelex.removeAnnotationModel(KEY);
    }

    /**
     * Handle event: Different coverage data was selected to be displayed.
     * Update model.
     */
    protected void selectionChanged() {
        updateAnnotations(true);
    }

    /**
     * Handle event: The coverage data displayed was changed. Update model.
     */
    protected void dataChanged() {
        updateAnnotations(true);
    }    
    
    /**
     * Find out if code can be annotated and what changed. Update annotations by
     * adding, removing or rebuilding them.
     * 
     * @param force
     *            always creates annotations
     */
    protected void updateAnnotations(boolean force) {
        boolean annotate = false;
        preconditions: {
            /* annotate only editor with changes saved */
            if (editor.isDirty()) break preconditions;
            
            /* only editors that display something */
            IEditorInput input = editor.getEditorInput();
            if (input == null) break preconditions;

            /* only editors with source code in active TSC */
            Object element = input.getAdapter(IJavaElement.class);
            if (element == null)
                /* no Java Editor */
                break preconditions;

            if (!hasSource((IJavaElement) element))
                /* no suitable source in active TSC */
                break preconditions;
            
            /* only annotate when test cases are activated */
            ActiveTSContainerInfo activeTSCInfo = getPlugin()
                    .getTSContainerManager().getActiveTSContainer();
            if (activeTSCInfo == null
                    || activeTSCInfo.getActiveTestCases().size() < 1) {
                break preconditions;
            }
            
            annotate = true;
        }
        if (annotate) {
            if (!annotated || force) {
                createAnnotations();
                annotated = true; 
            }
        } else {
            if (annotated) {
                clear();
                annotated = false; 
            }
        }
    }

    /**
     * @return the plugin instance for this model
     */
    private static CodeCoverPlugin getPlugin() {
        return CodeCoverPlugin.getDefault();
    }

    /**
     * Determine if the given element has a piece of source code inside the
     * active TSC.
     * 
     * @param element
     *            not null
     * @return true &rarr; the element has a piece of source code in the active
     *         tsc <br>
     *         false &rarr; it doesn't
     */
    protected boolean hasSource(IJavaElement element) {
        if (element == null) {
            throw new IllegalArgumentException("element is null"); //$NON-NLS-1$
        }

        TSContainerManager tscManager = getPlugin().getTSContainerManager();
        ActiveTSContainerInfo activeTSContainer = tscManager.getActiveTSContainer();
        if (activeTSContainer == null) {
            /* no test session container activated */
            return false;
        }
        TestSessionContainer tsc = activeTSContainer.getTestSessionContainer();

        codeFile = EclipseMASTLinkage.findSource(tsc.getCode(), element);

        if (codeFile != null) {

            //XXX: only in java we have 1 class ==> all in one file
            highlightedFile = EclipseMASTLinkage.MAST.getHighlightLocation(codeFile).getFile();
            
            /* code file must be the package to contain other classes in the
             * file.
             */
            codeFile = EclipseMASTLinkage.MAST.getPackage(codeFile, tsc);
            
            /* check for changes */
            String mastText, editorText;
            mastText = highlightedFile.getContent();
            editorText = document.get();
            
            return mastText.equals(editorText);
        }

        return false;
    }

    /**
     * Clears the annotations.
     */
    protected void clear() {
        AnnotationModelEvent event = new AnnotationModelEvent(this);
        clear(event);
        fireModelChanged(event);
    }

    /**
     * Clears annotations and adds them to the list of removed annotations
     * in the given {@link AnnotationModelEvent}, which must be fired, for
     * the editor to know about the change.
     * 
     * @param event
     *            the given {@link AnnotationModelEvent}
     */
    protected void clear(AnnotationModelEvent event) {
        for (Iterator<org.eclipse.jface.text.source.Annotation> i = annotations.iterator(); i.hasNext();) {
            EclPositionedAnnotation ca = (EclPositionedAnnotation) i.next();
            event.annotationRemoved(ca, ca.getPosition());
        }
        annotations.clear();
    }

    /**
     * This method calculates the coverage and generates eclipse annotations
     * from the result. 
     * <p>
     * codeFile and highlightedFile must be set properly (not null)
     */
    protected void createAnnotations() {
        
        AnnotationModelEvent event = new AnnotationModelEvent(this);
        clear(event);

        try {
            /* calculate Annotations (using code from HTML-Report) */
            
            /* prepare arguments for CodeHighlighting */
            CodeCoverPlugin plug = getPlugin();
            ActiveTSContainerInfo activeTSCInfo = plug.getTSContainerManager().getActiveTSContainer();
            List<TestCase> testCases;
            Vector<CoverageMetric> coverageMetrics = new Vector<CoverageMetric>();
            TestSessionContainer tsc;
            if (activeTSCInfo != null) {
                testCases = new ArrayList<TestCase>(
                        activeTSCInfo.getActiveTestCases());
                tsc = activeTSCInfo.getTestSessionContainer();
            } else {
                /* no tsc available */
                fireModelChanged(event); /* clear old Annotations */
                return;
            }
            coverageMetrics.addAll(MetricProvider.getAvailabeCoverageMetrics(
                    plug.getEclipsePluginManager().getPluginManager(),
                    plug.getLogger(), tsc.getCriteria()));
            if (coverageMetrics.size() < 1) {
                //only happens, when CodeCover is not built correctly
                plug.getLogger().error("Needed a some coverage metric, but got" //$NON-NLS-1$
                        + " none. Check required plugins."); //$NON-NLS-1$
            }
            CodeHighlighting highlighter = new CodeHighlighting(plug.getLogger());
            
            /* - codeFile is node of compilation unit and
             *   highlightedFile it's SourceFile
             * - testCases are the selected testcases
             * - coverageMetrics are all registered CoverageMetrics
             */
            
            Collection<CoverageAnnotation> ccAnnotations;
            ccAnnotations = highlighter.annotateCoverage(
                    codeFile, testCases, coverageMetrics, highlightedFile);

            /* translate annotations into Eclipse model*/ 
            for (CoverageAnnotation ca : ccAnnotations) {
                EclPositionedAnnotation eclCa = new EclCoverageAnnotation(ca);
                annotations.add((org.eclipse.jface.text.source.Annotation) eclCa);
                event.annotationAdded(eclCa);
            }
            List<CoverageAnnotation> ccAnnotationsList = null; 
            ccAnnotationsList = new Vector<CoverageAnnotation>(ccAnnotations);
            
            /* calculate corresponding line coverage for hot path */
            Collections.sort(ccAnnotationsList, Annotation.compareByStart);
            HighlightedSnippet snippet = highlighter.generateLineAnnotationsByFile(
                    ccAnnotationsList, highlightedFile.getContent());
            
            /* translate hot path annotations into Eclipse model */
            //XXX: global max over all files may be better, but is not in spec
            long maxExecutions = snippet.getExecutions();
            if (maxExecutions < 1) {
                maxExecutions = 1;
            }           
            for (LineExecutionAnnotation lea : snippet.getTextLines()) {
                if  (lea.hasExecutions()) {
                    EclPositionedAnnotation ca = new EclLineExecutionAnnotation(lea, maxExecutions);
                    annotations.add(ca);
                    event.annotationAdded(ca);
                }
            }
            
            fireModelChanged(event);
        } catch (RuntimeException re) {
            getPlugin().getLogger().error("annotating code failed", re); //$NON-NLS-1$
        }
    }

    @Override
	public void addAnnotationModelListener(IAnnotationModelListener listener) {
        if (!annotationModelListeners.contains(listener)) {
            annotationModelListeners.add(listener);
            fireModelChanged(new AnnotationModelEvent(this, true));
        }
    }

    @Override
	public void removeAnnotationModelListener(IAnnotationModelListener listener) {
        annotationModelListeners.remove(listener);
    }

    /**
     * Fires a change event for this model
     * 
     * @param event
     *            the event to be fired.
     */
    protected void fireModelChanged(AnnotationModelEvent event) {
        event.markSealed();
        if (!event.isEmpty()) {
            for (Iterator<IAnnotationModelListener> i = annotationModelListeners.iterator()
                    ; i.hasNext(); ) {
                IAnnotationModelListener l = i.next();
                if (l instanceof IAnnotationModelListenerExtension) {
                    ((IAnnotationModelListenerExtension) l).modelChanged(event);
                } else {
                    l.modelChanged(this);
                }
            }
        }
    }

    @Override
	public void connect(@SuppressWarnings("hiding")IDocument document) {
        if (this.document != document) 
            throw new RuntimeException("Can't connect to different document."); //$NON-NLS-1$
        for (org.eclipse.jface.text.source.Annotation ca : annotations) {
            try {
                document.addPosition(((EclCoverageAnnotation)ca).getPosition());
            } catch (BadLocationException ex) {
                getPlugin().getLogger().error("calculated invalid location for annotion", ex); //$NON-NLS-1$
            }
        }
        if (openConnections++ == 0) {
            /* update this model if it's source is modified */
            getPlugin().getTSContainerManager().addListener(modelListener);
            document.addDocumentListener(documentListener);
        }
    }

    @Override
	public void disconnect(@SuppressWarnings("hiding")IDocument document) {
        if (this.document != document) 
            throw new RuntimeException("Can't disconnect from different document."); //$NON-NLS-1$
        for (org.eclipse.jface.text.source.Annotation ca : annotations) {
            document.removePosition(((EclPositionedAnnotation)ca).getPosition());
        }
        if (--openConnections == 0) {
            getPlugin().getTSContainerManager().removeListener(modelListener);
            document.removeDocumentListener(documentListener);
        }
    }

    /**
     * External modification is not supported.
     */
    @Override
	public void addAnnotation(org.eclipse.jface.text.source.Annotation annotation,
            Position position) {
        throw new UnsupportedOperationException();
    }

    /**
     * External modification is not supported.
     */
    @Override
	public void removeAnnotation(org.eclipse.jface.text.source.Annotation annotation) {
        throw new UnsupportedOperationException();
    }

    @Override
	public Iterator<org.eclipse.jface.text.source.Annotation> getAnnotationIterator() {
        return annotations.iterator();
    }


    @Override
	public Position getPosition(org.eclipse.jface.text.source.Annotation annotation) {
        if (annotation instanceof EclPositionedAnnotation) {
            return ((EclPositionedAnnotation) annotation).getPosition();
        }
        
        //this code line is often reached in Eclipse 3.3
        return null;
    }

    /**
     * Listen for all changes in the model that may result in changed
     * annotations and force an update on them if so.
     */
    protected final class TSManagerListener implements
            TSContainerManagerListener {

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo, ChangeType, TestCase)
         */
        @Override
		public void testCaseChanged(ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestCase testCase) {
            /* some test case was changed/removed/added in active TSC */
            if (changeType == ChangeType.CHANGE
                    && getPlugin().getTSContainerManager()
                            .getActiveTSContainer().getActiveTestCases()
                                    .contains(testCase)) {
                /* activated test case changed */
                dataChanged();
            }
            /* relevant removed test cases are already handled via an implicit
             *  textCasesActiviated event. */
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(ActiveTSContainerInfo)
         */
        @Override
		public void testCasesActivated(ActiveTSContainerInfo tscInfo) {
            /* different test cases */
            selectionChanged();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(ActiveTSContainerInfo, ChangeType, TestSession)
         */
        @Override
		public void testSessionChanged(ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestSession testSession) {
            // We don't react on this.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
         */
        @Override
		public void testSessionContainerActivated(
                ActiveTSContainerInfo tscInfo) {
            /* got different TSC now */
            selectionChanged();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerAdded(org.codecover.eclipse.tscmanager.TSContainerInfo,
         *      int)
         */
        @Override
		public void testSessionContainerAdded(TSContainerInfo tscInfo, int index) {
            // We don't react on this.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerChanged(org.codecover.model.utils.ChangeType,
         *      org.codecover.eclipse.tscmanager.ActiveTSContainerInfo)
         */
        @Override
		public void testSessionContainerChanged(ChangeType changeType,
                ActiveTSContainerInfo tscInfo) {
            /* irrelevant change of list of test sessions in TSC */
            // We don't react on this.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerRemoved(org.codecover.eclipse.tscmanager.TSContainerInfo)
         */
        @Override
		public void testSessionContainerRemoved(TSContainerInfo tscInfo) {
            // We don't react on this.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#synchronizedStateChanged(TSContainerInfo, boolean)
         */
        @Override
		public void synchronizedStateChanged(TSContainerInfo tscInfo,
                boolean isSynchronized) {
            // We don't react on this.
        }
    }
}
