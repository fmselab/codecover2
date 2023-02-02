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

import static org.codecover.eclipse.utils.CodeCoverSorter.COMPARATOR_KEY;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManagerListener;
import org.codecover.eclipse.utils.CodeCoverSorter;
import org.codecover.eclipse.utils.DefaultSorterAndLabeler;
import org.codecover.eclipse.utils.IColumnSorterAndLabeler;
import org.codecover.eclipse.utils.ISorterAndLabeler;
import org.codecover.eclipse.utils.ImageProvider;
import org.codecover.eclipse.utils.InverseSorterAndLabeler;
import org.codecover.metrics.Metric;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.coverage.AbstractCoverageMetric;
import org.codecover.metrics.coverage.CoverageMetric;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.MergeException;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.IntComparator;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.criteria.Criterion;
import org.eclipse.core.resources.IProject;
import org.eclipse.jdt.ui.ISharedImages;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;

/**
 * The Coverage View displays the coverage results of the active
 * <code>TestSessionContainer</code>.
 *
 * @author Robert Hanussek, Markus Wittlinger
 * @version 1.0 ($Id: CoverageView.java 75 2011-06-28 13:28:30Z dobrowolsk $)
 */
public class CoverageView extends CodeCoverView
        implements TSContainerManagerListener {

   

    private static final String DIALOG_ERROR_INCONSISTENCY_TITLE = Messages
            .getString("CoverageView.DIALOG_ERROR_INCONSISTENCY_TITLE"); //$NON-NLS-1$

    private static final String DIALOG_ERROR_INCONSISTENCY_MSG = Messages
            .getString("CoverageView.DIALOG_ERROR_INCONSISTENCY_MSG"); //$NON-NLS-1$

    /*
     * some memento constants for saving the width of the columns
     */
    private static final String TAG_COLUMN_INDEX = "columnIndex"; //$NON-NLS-1$
    private static final String TAG_TYPE = "columnInfo";          //$NON-NLS-1$
    private static final String TAG_COLUMN_WIDTH = "columnWidth"; //$NON-NLS-1$
    private static final String TAG_COLUMN_NAME = "columnName";   //$NON-NLS-1$

    /*
     * fields which save the data which is visualized in the view
     */

    

    

    private final List<CoverageMetric> sortedCoverageMetrics;

    /*
     * fields of the GUI components of the view
     */

    private TreeViewer viewer;
    private GroupByActionsManager groupByActions;

    // filter and controls for coverage filtering
    private ViewerCoverageFilter viewerCovFilter;
    private Button bttCovFilterToggle;
    private Combo cmbCovFilterMetric;
    private Combo cmbCovFilterCompOp;
    private Text txtCovFilterPercent;
    private Label lblCovFilterPercent;

    /**
     * The IDs of the expanded HierarchyLevels of the associated
     * <code>TSContainerInfo</code>-representation of a test session container.
     */
    private Map<TSContainerInfo, Set<String>> expandedHLevIDs;

    private IMemento memento;

    private final Logger logger;

    /** this cache contains coverage results mapped by {@link HierarchyLevel} in the order of the metrics. */
    private Map<HierarchyLevel, CoverageResult>[] coverageCache;

    /*
     * classes/methods to construct, initialize and create the view
     */

    /**
     * The constructor.
     */
    public CoverageView() {
        this.logger = CodeCoverPlugin.getDefault().getLogger();

        this.expandedHLevIDs = new HashMap<TSContainerInfo, Set<String>>();

        this.queueLock = new Object();
        this.updateLock = new Object();
        this.activeTSCInfo = null;
        this.queuedActiveTSCInfo = null;
        this.updatePending = false;

        /*
         * this.groupByActions Object is created in method createPartControl
         * because getViewSite() could return null here
         */

        this.sortedCoverageMetrics = CoverageView.getCoverageMetrics();
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite,
     *      org.eclipse.ui.IMemento)
     */
    @Override
    public void init(IViewSite site, IMemento memento) throws PartInitException {
        super.init(site, memento);
        this.memento = memento;
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        parent.setLayout(new FormLayout());

        // create the manager which will handle the "Group By"-buttons
        this.groupByActions = new GroupByActionsManager(Type.PROJECT,
                getViewSite().getActionBars().getToolBarManager());

        /*
         * create and layout coverage filtering controls
         */
        this.createCoverageFilterControls(parent);
        this.layoutCoverageFilterControls();

        /*
         * create tree viewer and set its layout
         */
        this.createTreeViewer(parent);
        this.layoutTreeViewer();

        /*
         * create name filtering controls and layout them
         */
        /*
         * Since the implementation of name filtering runs much too slow, it was
         * commented out. It wasn't deleted since an enhanced version (e.g.,
         * by using caching) could be used in future releases.
         */
        //this.createNameFilterControls(parent);
        //this.layoutNameFilterControls();

        /*
         * create "Group By"-Buttons in the toolbar of the view
         */
        this.createGroupByControls(parent);

        /*
         * create double click action which opens an editor with the code of
         * the double-clicked element
         */
        this.hookDoubleClickAction();

        /*
         * set the input of the viewer to the (content of the) currently active
         * test session container
         */
        this.setViewerInput(CodeCoverPlugin.getDefault()
                        .getTSContainerManager().getActiveTSContainer());

        /*
         * register listener on TSContainerManager
         */
        CodeCoverPlugin.getDefault().getTSContainerManager().addListener(this);
    }

    /*
     * methods to create and layout the tree viewer
     */

    private void createTreeViewer(Composite parent) {
        // create a TreeViewer
        this.viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL
                | SWT.V_SCROLL | SWT.BORDER);
        // setup columns of the tree viewer
        this.createTableColumns();
        // set content and label providers and the sorter
        this.viewer.setContentProvider(new ViewContentProvider());
        this.viewer.setLabelProvider(new ViewLabelProvider());
        this.viewer.setSorter(new CodeCoverSorter(this.viewer,
                this.viewer.getTree().getColumns()));
    }

    private void layoutTreeViewer() {
        FormData formData = new FormData();
        formData.left = new FormAttachment(0, 5);
        formData.right = new FormAttachment(100, -5);
        formData.top = new FormAttachment(this.cmbCovFilterMetric, 5);
        formData.bottom = new FormAttachment(100, -5);
        this.viewer.getControl().setLayoutData(formData);
    }

    /*
     * methods to set and get the data which is visualized by this view
     */

    

    @Override
    protected void performUpdate() {
        boolean performFullUpdate = false;
        synchronized(this.updateLock) {
            synchronized(this.queueLock) {
                /*
                 * Check if we have to perform a full update.
                 * A full update must be performed if another test session
                 * container was activated. A simple refresh is performed if the
                 * test session container is still the same, but the set of
                 * active test cases changed.
                 */
                if ((   this.queuedActiveTSCInfo != null && this.activeTSCInfo == null)
                     || (this.queuedActiveTSCInfo == null && this.activeTSCInfo != null)
                     || (this.queuedActiveTSCInfo != null && this.activeTSCInfo != null
                         && this.queuedActiveTSCInfo.getTestSessionContainer() != this.activeTSCInfo.getTestSessionContainer())) {
                    performFullUpdate = true;
                }
                /*
                 * save the expanded elements if we're going to perform a full
                 * update and the old TSC is non-null
                 */
                if (performFullUpdate && this.getVisTSCInfo() != null) {
                    this.saveExpandedElements(this.getVisTSCInfo());
                }
                this.activeTSCInfo = this.queuedActiveTSCInfo;
                this.queuedActiveTSCInfo = null;
                this.updatePending = false;
            }

            // the current coverage cache is not usable anymore because the content has changed
            this.coverageCache = null;

            if (performFullUpdate) {
                this.performFullUpdate();
            } else {
                this.performTestCaseRefresh();
            }
        }
    }

    private void performFullUpdate() {
        synchronized(this.updateLock) {
            if(!this.viewer.getControl().isDisposed()) {
                /*
                 * The selection is set to EMPTY to prevent a bug/feature of the
                 * tree viewer (StructuredViewer/AbstractTreeViewer) which tries
                 * to restore the selection even after a call to setInput (which
                 * can change the content of the viewer totally). The problem
                 * is that the tree viewer seems to buffer the objects
                 * (HierarchyLevels) which are selected before the call to
                 * setInput and then tries to restore the selection by looking
                 * for the elements in the content provider of the viewer, which
                 * includes calls to ITreeContentProvider.getParent which in
                 * turn calls TestSessionContainer.getParentOfHierarchyLevel of
                 * the newly set (via setInput) TSC. The latter doesn't like
                 * getting requests for HierarchyLevels which don't belong to
                 * its test session container (they belong to the old TSC which
                 * was active before the call to setInput) and throws an
                 * exception.
                 */
                this.viewer.setSelection(StructuredSelection.EMPTY);
                this.viewer.setInput(this.getVisTSC());
                if(this.getVisActiveTSCInfo() != null) {
                    this.restoreExpandedElements(this.getVisActiveTSCInfo());
                }
            }
        }
    }

    private void performTestCaseRefresh() {
        synchronized(this.updateLock) {
            if(!this.viewer.getControl().isDisposed()) {
                Object[] expandedElements = this.viewer.getExpandedElements();
                this.viewer.refresh();
                this.viewer.setExpandedElements(expandedElements);
            }
        }
    }

    private ActiveTSContainerInfo getVisActiveTSCInfo() {
        synchronized(this.updateLock) {
            return this.activeTSCInfo;
        }
    }



    /**
     * Returns the <code>TestCase</code>s which are currently visualized by this
     * view.
     *
     * @return  the <code>TestCase</code>s which are currently visualized by
     *          this view.
     */
    private Set<TestCase> getVisTestCases() {
        synchronized(this.updateLock) {
            return (this.activeTSCInfo != null) ?
                    this.activeTSCInfo.getActiveTestCases()
                    : null;
        }
    }

    /**
     * Returns the <code>IProject</code> the test session container, which is
     * visualized in this view, belongs to.
     *
     * @return  the <code>IProject</code> the test session container, which is
     *          visualized in this view, belongs to.
     */
    private IProject getVisProject() {
        synchronized (this.updateLock) {
            return (this.activeTSCInfo != null) ? this.activeTSCInfo.getProject() : null;
        }
    }

    private void createTableColumns() {
        Tree tree = this.viewer.getTree();
        tree.setRedraw(false);
        tree.setHeaderVisible(true);
        TreeColumn column = new TreeColumn(tree, SWT.LEFT);
        IColumnSorterAndLabeler<?> nameColumnSorterAndLabeler = makeNameColumnSorterAndLabeler();
        column.setText(nameColumnSorterAndLabeler.getColumnName());
        column.setData(CodeCoverSorter.COMPARATOR_KEY, nameColumnSorterAndLabeler);
        column.setWidth(220);

        for (int i = 0; i < this.sortedCoverageMetrics.size(); i++) {
            CoverageMetric metric = this.sortedCoverageMetrics.get(i);
            column = new TreeColumn(tree, SWT.LEFT);
            column.setWidth(160);
            column.setData(metric);
            IColumnSorterAndLabeler<?> metricColumnSorterAndLabeler = makeCoverageSorterAndLabeler(metric, i);
            column.setText(metricColumnSorterAndLabeler.getColumnName());
            column.setData(CodeCoverSorter.COMPARATOR_KEY, metricColumnSorterAndLabeler);
            column.pack();
            // at least 80px
            column.setWidth(Math.max(column.getWidth(), 80));
        }

        if (this.memento != null) {
            IMemento[] mementos = this.memento.getChildren(TAG_TYPE);
            for (IMemento currentMemento : mementos) {
                Integer columnIndex = currentMemento.getInteger(TAG_COLUMN_INDEX);
                Integer columnWidth = currentMemento.getInteger(TAG_COLUMN_WIDTH);
                String columnName = currentMemento.getString(TAG_COLUMN_NAME);

                if (columnIndex != null && columnWidth != null && columnName != null
                      && columnIndex.intValue() < tree.getColumnCount()) {
                    TreeColumn treeColumn = tree.getColumn(columnIndex.intValue());
                    treeColumn.setWidth(columnWidth);
                }
            }
        }

        tree.setRedraw(true);
    }


    /**
     * Gets all the {@link CoverageMetric}s
     *
     * @return the {@link Set} of {@link CoverageMetric}s
     */
    private static List<CoverageMetric> getCoverageMetrics() {
        Set<Metric> allMetrics = MetricProvider.getAvailabeMetrics(CodeCoverPlugin.getDefault().
                getEclipsePluginManager().getPluginManager(), CodeCoverPlugin.getDefault().getLogger());
        List<CoverageMetric> coverageMetrics = new ArrayList<CoverageMetric>();
        List<CoverageMetric> unknownCovMetrics = new ArrayList<CoverageMetric>();

        // add known metrics
        coverageMetrics.add(findCoverageMetric("Statement Coverage", allMetrics));                //$NON-NLS-1$
        coverageMetrics.add(findCoverageMetric("Branch Coverage", allMetrics));                   //$NON-NLS-1$
        coverageMetrics.add(findCoverageMetric("Loop Coverage", allMetrics));                     //$NON-NLS-1$
        coverageMetrics.add(findCoverageMetric("Term Coverage", allMetrics));         //$NON-NLS-1$
        coverageMetrics.add(findCoverageMetric("?-Operator Coverage", allMetrics));         //$NON-NLS-1$
        coverageMetrics.add(findCoverageMetric("Synchronized Coverage", allMetrics));         //$NON-NLS-1$
 
        // fetch unknown coverage metrics
        for (Metric metric : allMetrics) {
            if (metric instanceof CoverageMetric && !coverageMetrics.contains(metric)) {
                unknownCovMetrics.add((CoverageMetric)metric);
            }
        }

        // sort unknown metrics lexicographically (case insensitive)
        Collections.sort(unknownCovMetrics, new Comparator<CoverageMetric>() {
            @Override
			public int compare(CoverageMetric cm1, CoverageMetric cm2) {
                return cm1.getName().compareToIgnoreCase(cm2.getName());
            }
        });

        // merge known and unknown metrics and return them
        coverageMetrics.addAll(unknownCovMetrics);
        return coverageMetrics;
    }

    /**
     * Searches the given collection of <code>Metric</code>s for a
     * <code>CoverageMetric</code> with the given name.
     *
     * @param name      the name of the <code>CoverageMetric</code> to search
     *                  for
     * @param metrics   the collection to search
     *
     * @return  the first found <code>CoverageMetric</code> which has the given
     *          name
     */
    private static CoverageMetric findCoverageMetric(String name, Collection<Metric> metrics) {
        for(Metric metric : metrics) {
            if(metric instanceof CoverageMetric && metric.getName().equals(name)) {
                return (CoverageMetric)metric;
            }
        }
        return null;
    }

    /**
     * Removes the suffix &quot; Coverage&quot; from the given
     * <code>String</code> if it ends with this suffix.
     *
     * @param name  the <code>String</code> to remove the suffix
     *              &quot; Coverage&quot; from
     *
     * @return  the given <code>String</code> with the suffix removed, or the
     *          given <code>String</code> without any change if it didn't end
     *          with the suffix &quot; Coverage&quot; in the first place
     */
    private static String stripCoverageMetricName(String name) {
        final String suffix = " Coverage";                         //$NON-NLS-1$
        if(name.endsWith(suffix)) {
            return name.substring(0, name.length()-suffix.length());
        } else {
            return name;
        }
    }

    /**
     * Retrieves the coverage cache or makes a new calculation if the cache is null and returns the
     * {@link CoverageResult} for a {@link HierarchyLevel}.
     */
    @SuppressWarnings("unchecked")
    private CoverageResult calculatedCoverage(int metricIndex, HierarchyLevel level) {
        synchronized (this.updateLock) {
            if (this.coverageCache == null) {
                this.coverageCache = new Map[0];

                TestSessionContainer tsc = getVisTSC();
                Set<TestCase> testCases = getVisTestCases();

                // we create a new cache
                if (tsc != null && !testCases.isEmpty()) {
                    // get all metrics where are plug-ins available and that are supported
                    // by VisTSC
                    Set<CoverageMetric> metricForCalculation = new HashSet<CoverageMetric>();
                    Set<Criterion> providedCriteria = tsc.getCriteria();

                    for (CoverageMetric metric : this.sortedCoverageMetrics) {
                        if (providedCriteria.containsAll(metric.getRequiredCriteria())) {
                            metricForCalculation.add(metric);
                        }
                    }

                    if (metricForCalculation.isEmpty()) {
                        this.logger.debug("No Available Metrics for current TSC (ID: " //$NON-NLS-1$
                                + tsc.getId() + ")."); //$NON-NLS-1$
                        this.logger.debug("Class path: " + System.getProperty("java.class.path")); //$NON-NLS-1$ //$NON-NLS-2$
                    } else {
                        // we try to merge the coverage into a single test case to allow more performant
                        // calculation within the coverage metric calculation
                        Set<TestCase> testCasesToUse = testCases;
                        try {
                            TestCase mergedCoverageTestCase = TestCase.mergeCoverageToTemporaryTestCase(testCases);
                            testCasesToUse = Collections.singleton(mergedCoverageTestCase);
                        } catch (MergeException e) {
                            // ignore
                        }

                        // we calculate the coverage for each metric that is supported by the criteria
                        this.coverageCache = new Map[this.sortedCoverageMetrics.size()];

                        long tsBefore = System.currentTimeMillis();
                        int index = 0;
                        long totalCoverableItems = 0;
                        for (CoverageMetric metric : this.sortedCoverageMetrics) {
                            if (metricForCalculation.contains(metric)) {
                                // the metric is supported
                                this.coverageCache[index] = AbstractCoverageMetric.calculateCoverageForAllLevels(metric,
                                        testCasesToUse, tsc.getCode());
                                totalCoverableItems += this.coverageCache[index].get(tsc.getCode()).getTotalItems();
                            }
                            index++;
                        }
                        long duration = System.currentTimeMillis() - tsBefore;
                        this.logger.info("Calculated coverage for " + index + " metrics, " + testCases.size() //$NON-NLS-1$ //$NON-NLS-2$
                                + " test cases and " + totalCoverableItems + " coverable items in " + duration + " ms"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
                    }
                }
            }
        }

        // we can get the result from the cache
        if (metricIndex < 0 || metricIndex >= this.coverageCache.length || this.coverageCache[metricIndex] == null) {
            return null;
        }
        return this.coverageCache[metricIndex].get(level);
    }

    /*
     * classes which connect the (tree) viewer with the
     * data model/TSContainerManager
     */

    private class ViewContentProvider
            implements IStructuredContentProvider, ITreeContentProvider {
        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
            // Do nothing.
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        @Override
		public void dispose() {
            CodeCoverPlugin.getDefault().getTSContainerManager().removeListener(CoverageView.this);
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        @Override
		public Object[] getElements(Object parent) {
            if (parent instanceof TestSessionContainer) {
                if (CoverageView.this.groupByActions.getRootType() == Type.PROJECT) {
                    return oldestChildrenOfType(Type.PROJECT, ((TestSessionContainer) parent).getCode())
                            .toArray();
                } else if (CoverageView.this.groupByActions.getRootType() == Type.PACKAGE) {
                    return oldestChildrenOfType(Type.PACKAGE, ((TestSessionContainer) parent).getCode())
                            .toArray();
                } else if (CoverageView.this.groupByActions.getRootType() == Type.CLASS) {
                    return oldestChildrenOfType(Type.CLASS, ((TestSessionContainer) parent).getCode())
                            .toArray();
                } else if (CoverageView.this.groupByActions.getRootType().equals(Type.METHOD)) {
                    return oldestChildrenOfType(Type.METHOD, ((TestSessionContainer) parent).getCode())
                            .toArray();
                } else {
                    return ((TestSessionContainer) parent).getCode().getChildren().toArray();
                }
            } else {
                CoverageView.this.logger.error("Unknown object injected into tree viewer of Coverage view."); //$NON-NLS-1$
                return new Object[0];
            }
        }

        private List<HierarchyLevel> oldestChildrenOfType(Type rootType,
                HierarchyLevel child) {
            ArrayList<HierarchyLevel> children = new ArrayList<HierarchyLevel>(128);
            if (Type.typeOf(child) == rootType) {
                children.add(child);
                children.trimToSize();
                return children;
            }
            for (HierarchyLevel hLev : child.getChildren()) {
                children.addAll(oldestChildrenOfType(rootType, hLev));
            }
            children.trimToSize();
            return children;
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
         */
        @Override
		public Object getParent(Object child) {
            TestSessionContainer tsc = CoverageView.this.getVisTSC();
            HierarchyLevel parent;
            if (child instanceof TestSessionContainer) {
                // this case is handled by getElements
                return null;
            } else if(child instanceof HierarchyLevel) {
                // tsc == null actually happens while remove the active TSC
                if(tsc == null || tsc.getCode() == null) {
                    return null;
                }
                try {
                    parent
                        = tsc.getParentOfHierarchyLevel((HierarchyLevel)child);
                } catch(IllegalArgumentException e) {
                    CoverageView.this.logger.error("Possible inconsistency in Coverage view", e); //$NON-NLS-1$

                    Display.getDefault().asyncExec(new Runnable() {
                        @Override
						public void run() {
                            MessageDialog.openError(
                                    CoverageView.this.getSite().getShell(),
                                    DIALOG_ERROR_INCONSISTENCY_TITLE,
                                    DIALOG_ERROR_INCONSISTENCY_MSG);
                        }
                    });

                    return null;
                }

                if(parent != null
                        /*
                         * ... && the type of hierarchyLevel is equal or smaller
                         *        than the root type
                         */
                        && Type.typeOf(parent).compareTo(
                                CoverageView.this.groupByActions.getRootType())
                                        <= 0) {
                    return parent;
                } else {
                    return null;
                }
            }
            return null;
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
         */
        @Override
		public Object[] getChildren(Object parent) {
            if (parent instanceof HierarchyLevel) {
                return ((HierarchyLevel) parent).getChildren().toArray();
            } else {
                CoverageView.this.logger.error(
                        "Unknown object contained in tree" +       //$NON-NLS-1$
                        " viewer of Coverage view.");              //$NON-NLS-1$
                return new Object[0];
            }
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
         */
        @Override
		public boolean hasChildren(Object parent) {
            if(parent instanceof HierarchyLevel) {
                return !(((HierarchyLevel) parent).getChildren().isEmpty());
            }
            return false;
        }

    }

    private static final DecimalFormat PERCENTAGE_FORMAT = new DecimalFormat("0.0 %"); //$NON-NLS-1$

    private static final DecimalFormat ABSOLUTE_FORMAT = new DecimalFormat("#,##0"); //$NON-NLS-1$

    /**
     * Checks the given {@link CoverageResult} and returns the float value of it's quotient
     *
     * @param result
     *            the given {@link CoverageResult}
     * @return result.getCoveredItems() / result.getTotalItems(), or {@link Float#NaN} if
     *         result.getTotalItems() == 0
     */
    private static float evaluateCoverageResult(CoverageResult result) {
        float coverage = Float.NaN;
        if (result.getTotalItems() > 0) {
            coverage = ((float) result.getCoveredItems() / result.getTotalItems());
        }
        return coverage;
    }

    private class ViewLabelProvider extends LabelProvider
            implements ITableLabelProvider {

        /**
         * (non-Javadoc)
         *
         * @see ITableLabelProvider#getColumnText(Object, int)
         */
        @Override
		@SuppressWarnings("unchecked")
        public String getColumnText(Object obj, int index) {
            IColumnSorterAndLabeler<? super Object> labeler =
                (IColumnSorterAndLabeler<? super Object>) CoverageView.this.viewer.getTree().getColumn(index).getData(COMPARATOR_KEY);
            return labeler.getLabelText(obj);
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object,
         *      int)
         */
        @Override
		@SuppressWarnings("unchecked")
        public Image getColumnImage(Object obj, int index) {
            IColumnSorterAndLabeler<? super Object> labeler =
                (IColumnSorterAndLabeler<? super Object>) CoverageView.this.viewer.getTree().getColumn(index).getData(COMPARATOR_KEY);
            return labeler.getColumnImage(obj);
        }
    }

    /**
     * Creates an instance of {@link IColumnSorterAndLabeler} for the {@link #COL_NAME}.
     *
     * @param metricIndex The index of the metric in {@link #sortedCoverageMetrics}.
     */
    @SuppressWarnings("unchecked")
    private IColumnSorterAndLabeler <HierarchyLevel> makeCoverageSorterAndLabeler(CoverageMetric metric, int metricIndex) {
        // 1) we have an percentage sorter
        ISorterAndLabeler<HierarchyLevel> percentageSorter = new CoverageResultSorterAndLabeler(metric, metricIndex) {
            @Override
            public int compare(CoverageResult o1, CoverageResult o2) {
                return Float.compare(evaluateCoverageResult(o1), evaluateCoverageResult(o2));
            }

            @Override
            public String getLabelText(CoverageResult result) {
                if (result.getTotalItems() == 0) {
                    return ""; //$NON-NLS-1$
                }
                float coverage = evaluateCoverageResult(result);
                return PERCENTAGE_FORMAT.format(coverage);
            }
        };

        // 2) this sorter is just the inverse to the percentageSorter
        ISorterAndLabeler<HierarchyLevel> invPercentageSorter = new InverseSorterAndLabeler<HierarchyLevel>(percentageSorter);

        // 3) we have an absolute sorter
        ISorterAndLabeler<HierarchyLevel> absoluteSorter = new CoverageResultSorterAndLabeler(metric, metricIndex) {
            @Override
            public int compare(CoverageResult o1, CoverageResult o2) {
                return IntComparator.compare(o1.getCoveredItems(), o2.getCoveredItems());
            }

            @Override
            public String getLabelText(CoverageResult result) {
                return ABSOLUTE_FORMAT.format(result.getCoveredItems()) + "/" //$NON-NLS-1$
                        + ABSOLUTE_FORMAT.format(result.getTotalItems());
            }

            @Override
            public String getColumnName() {
                return super.getColumnName() + Messages.getString("CoverageView.covered"); //$NON-NLS-1$
            }
        };

        // 4) this sorter is just the inverse to the absoluteSorter
        ISorterAndLabeler<HierarchyLevel> invAbsoluteSorter = new InverseSorterAndLabeler<HierarchyLevel>(absoluteSorter);

        // 5) we have an absolute remaining items sorter
        ISorterAndLabeler<HierarchyLevel> remainingSorter = new CoverageResultSorterAndLabeler(metric, metricIndex) {
            @Override
            public int compare(CoverageResult o1, CoverageResult o2) {
                return IntComparator.compare(o1.getTotalItems() - o1.getCoveredItems(), o2.getTotalItems() - o2.getCoveredItems());
            }

            @Override
            public String getLabelText(CoverageResult result) {
                return ABSOLUTE_FORMAT.format(result.getTotalItems() - result.getCoveredItems())
                  + "/" + ABSOLUTE_FORMAT.format(result.getTotalItems()); //$NON-NLS-1$
            }

            @Override
            public String getColumnName() {
                return super.getColumnName() + Messages.getString("CoverageView.remaining"); //$NON-NLS-1$
            }
        };

        // 6) this sorter is just the inverse to the remainingSorter
        ISorterAndLabeler<HierarchyLevel> invRemainingSorter = new InverseSorterAndLabeler<HierarchyLevel>(remainingSorter);

        return DefaultSorterAndLabeler.constructComparator(percentageSorter, invPercentageSorter,
                absoluteSorter, invAbsoluteSorter, remainingSorter, invRemainingSorter);
    }

    private abstract class CoverageResultSorterAndLabeler implements ISorterAndLabeler<HierarchyLevel> {

        private final CoverageMetric metric;
        private final int metricIndex;

        /**
         * @param metricIndex The index of the metric in {@link #sortedCoverageMetrics}.
         */
        public CoverageResultSorterAndLabeler(CoverageMetric metric, int metricIndex) {
            this.metric = metric;
            this.metricIndex = metricIndex;
        }

        @Override
		public int compare(HierarchyLevel hLev1, HierarchyLevel hLev2) {
            CoverageResult o1 = calculateResult(hLev1);
            CoverageResult o2 = calculateResult(hLev2);

            if (o1 == null && o2 == null) {
                return 0;
            } else if (o1 == null && o2 != null) {
                return 1;
            } else if (o1 != null && o2 == null) {
                return -1;
            } else if (o1.getTotalItems() == 0 && o2.getTotalItems() == 0) {
                return 0;
            } else if (o1.getTotalItems() != 0 && o2.getTotalItems() == 0) {
                return -1;
            } else if (o1.getTotalItems() == 0 && o2.getTotalItems() != 0) {
                return 1;
            } else {
                return compare(o1, o2);
            }
        }

        public abstract int compare(CoverageResult o1, CoverageResult o2);

        @Override
		public String getLabelText(HierarchyLevel hLev) {
            CoverageResult result = calculateResult(hLev);

            if (result == null) {
                return "?"; //$NON-NLS-1$
            }

            return getLabelText(result);
        }

        public abstract String getLabelText(CoverageResult result);

        @Override
		public Image getColumnImage(HierarchyLevel hLev) {
            CoverageResult result = calculateResult(hLev);

            if (result == null) {
                return null;
            }

            return ImageProvider.generateCoverageIndicator(result);
        }

        @Override
		public String getColumnName() {
            return stripCoverageMetricName(this.metric.getName());
        }

        @Override
		public int getSortDirection() {
            return SWT.UP;
        }

        private CoverageResult calculateResult(HierarchyLevel hLev)
        {
            return calculatedCoverage(this.metricIndex, hLev);
        }
    }

    /** Creates an instance of {@link IColumnSorterAndLabeler} for the {@link #COL_NAME}. */
    @SuppressWarnings("unchecked")
    private IColumnSorterAndLabeler<HierarchyLevel> makeNameColumnSorterAndLabeler() {
        ISorterAndLabeler<HierarchyLevel> baseSorter = new ISorterAndLabeler<HierarchyLevel>() {
            @Override
			public int compare(HierarchyLevel o1, HierarchyLevel o2) {
                if (o1 == null && o2 == null) {
                    return 0;
                } else if (o1 == null && o2 != null) {
                    return 1;
                } else if (o1 != null && o2 == null) {
                    return -1;
                } else {
                    return o1.getName().compareTo(o2.getName());
                }
            }

            @Override
			public String getLabelText(HierarchyLevel hLev) {
                if (Type.typeOf(hLev) == Type.PROJECT) {
                    return CoverageView.this.getVisProject().getName();
                } else {
                    return hLev.getName();
                }
            }

            @Override
			public Image getColumnImage(HierarchyLevel hLev) {
                /*
                 * Return icons for the name column dependent on the type of
                 * given HierarchyLevel. The default package is visualized as
                 * the (top-level) project node (with a project icon).
                 */
                if (hLev.getType().getInternalName().equals(DEFAULT_PACKAGE_NAME)) /* project */{
                    return PlatformUI.getWorkbench().getSharedImages().getImage(
                            IDE.SharedImages.IMG_OBJ_PROJECT);
                } else if (hLev.getType().getInternalName().equals(PACKAGE_NAME)) {
                    return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_PACKAGE);
                } else if (hLev.getType().getInternalName().equals(CLASS_NAME)) {
                    return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_CLASS);
                } else if (hLev.getType().getInternalName().equals(INTERFACE_NAME)) {
                    return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_INTERFACE);
                } else if (hLev.getType().getInternalName().equals(ENUM_NAME)) {
                    return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_ENUM);
                } else if (hLev.getType().getInternalName().equals(ANNOTATION_NAME)) {
                    return JavaUI.getSharedImages().getImage(ISharedImages.IMG_OBJS_ANNOTATION);
                } else if (hLev.getType().getInternalName().equals(METHOD_NAME)) {
                    return CodeCoverPlugin.getDefault().getImageRegistry().get(
                            CodeCoverPlugin.Image.METHOD.getPath());
                }
                return null;
            }

            @Override
			public String getColumnName() {
                return Messages.getString("CoverageView.15"); //$NON-NLS-1$
            }

            @Override
			public int getSortDirection() {
                return SWT.UP;
            }
        };

        // this sorter is just the inverse to the base
        ISorterAndLabeler<HierarchyLevel> invertedSorter = new InverseSorterAndLabeler<HierarchyLevel>(baseSorter);

        return DefaultSorterAndLabeler.constructComparator(baseSorter, invertedSorter);
    }

    /*
     * methods to create the double click action which opens an editor with the
     * code of the double-clicked element
     */

    /**
     * register action to open editor on a double click
     */
    private void hookDoubleClickAction() {
        this.viewer.addDoubleClickListener(new IDoubleClickListener() {
            @Override
			public void doubleClick(DoubleClickEvent event) {
                ISelection sel;
                Object element;
                if((sel = CoverageView.this.viewer.getSelection())
                        instanceof IStructuredSelection) {
                    element = ((IStructuredSelection)sel).getFirstElement();
                    if(element != null && element instanceof HierarchyLevel) {
                        /* element is the selected hierarchy level */
                        HierarchyLevel hLev = (HierarchyLevel)element;

                        /*
                         * only react on classes, interfaces, enums,
                         * annotations and methods
                         */
                        if (Type.typeOf(hLev) == Type.CLASS
                                || Type.typeOf(hLev) == Type.METHOD) {
                            showHierarchyLevelInEditor(hLev);
                        }
                    }
                }
            }
        });
    }

   

    /*
     * method and class for creating the buttons which can group the content
     * of the tree viewer by a specific HierarchyLevelType, e.g packages or
     * classes
     */

    private void createGroupByControls(Composite parent) {
        this.groupByActions.makeGroupByAction(Type.PROJECT,
                Messages.getString("CoverageView.3"), //$NON-NLS-1$
                Messages.getString("CoverageView.4"), //$NON-NLS-1$
                Messages.getString("CoverageView.5"), //$NON-NLS-1$
                PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
                        IDE.SharedImages.IMG_OBJ_PROJECT));
        this.groupByActions.makeGroupByAction(Type.PACKAGE,
                Messages.getString("CoverageView.6"), //$NON-NLS-1$
                Messages.getString("CoverageView.7"), //$NON-NLS-1$
                Messages.getString("CoverageView.8"), //$NON-NLS-1$
                JavaUI.getSharedImages().getImageDescriptor(
                        ISharedImages.IMG_OBJS_PACKAGE));
        this.groupByActions.makeGroupByAction(Type.CLASS,
                Messages.getString("CoverageView.9"), //$NON-NLS-1$
                Messages.getString("CoverageView.10"), //$NON-NLS-1$
                Messages.getString("CoverageView.11"), //$NON-NLS-1$
                JavaUI.getSharedImages().getImageDescriptor(
                        ISharedImages.IMG_OBJS_CLASS));
        this.groupByActions.makeGroupByAction(Type.METHOD,
                Messages.getString("CoverageView.12"), //$NON-NLS-1$
                Messages.getString("CoverageView.13"), //$NON-NLS-1$
                Messages.getString("CoverageView.14"), //$NON-NLS-1$
                CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.METHOD.getPath()));
    }

    /**
     * Handles the actions which allow the user to group the content of the code
     * tree (displayed in the tree viewer) by a specific {@link Type},
     * e.g packages or classes.
     */
    class GroupByActionsManager {

        final Map <Type, Action> actions;

        private final IToolBarManager toolBarManager;

        /**
         * Selected root type.
         */
        Type rootType;

        /**
         * Constructor
         *
         * @param initialRootType
         * @param toolBarManager
         */
        public GroupByActionsManager (Type initialRootType,
                IToolBarManager toolBarManager) {
            this.rootType = initialRootType;
            this.toolBarManager = toolBarManager;
            this.actions = new HashMap<Type, Action>();
        }

        /**
         * Adds the action to this manager, as well as the toolbar.
         *
         * @param rootType
         *            the {@link Type} of the action
         * @param action
         *            the {@link Action} to add.
         */
        public void addGroupByAction(Type rootType, Action action) {
            this.actions.put(rootType, action);
            // add the action to the views toolbar
            this.toolBarManager.add(action);
        }

        /**
         * Creates a groupByAction with the given parameters.
         *
         * @param rootType
         *            the {@link Type} of the action
         * @param text
         *            the text of the action
         * @param description
         *            the description of the action
         * @param toolTipText
         *            the tooltip of the action
         * @param icon
         *            the icon of the action
         */
        public void makeGroupByAction( final Type rootType,
                                        final String text,
                                        final String description,
                                        final String toolTipText,
                                        final ImageDescriptor icon) {
            Action newAction = new Action(text, IAction.AS_CHECK_BOX) {
                @Override
                public void run() {
                    if(!GroupByActionsManager.this.rootType.equals(rootType)) {
                        GroupByActionsManager.this.rootType = rootType;
                        // uncheck all root type actions
                        for(Action action :
                                GroupByActionsManager.this.actions.values()) {
                            action.setChecked(false);
                        }

                        // refresh viewer and resort
                        CoverageView.this.viewer.refresh();
                    }
                    // check button for this root type action
                    this.setChecked(true);
                }
            };
            newAction.setDescription(description);
            newAction.setToolTipText(toolTipText);

            newAction.setImageDescriptor(icon);
            this.addGroupByAction(rootType, newAction);
            // check button if this is the initial root type
            if(newAction == this.getGroupByAction(this.getRootType())) {
                newAction.setChecked(true);
            }
        }

        /**
         * Gets the rootType
         *
         * @return the {@link Type}
         */
        public Type getRootType() {
            return this.rootType;
        }

        /**
         * Gets the actions for the given {@link Type}
         *
         * @param rootType
         *            the given {@link Type}
         * @return the actions.
         */
        public Action getGroupByAction(Type rootType) {
            return this.actions.get(rootType);
        }
    }

    /*
     * The following constants are the internal names of all types.
     */
    static final String DEFAULT_PACKAGE_NAME
             = "default package";                              //$NON-NLS-1$
    static final String PACKAGE_NAME = "package";      //$NON-NLS-1$
    static final String CLASS_NAME = "class";          //$NON-NLS-1$
    static final String INTERFACE_NAME = "interface";  //$NON-NLS-1$
    static final String ENUM_NAME = "enum";            //$NON-NLS-1$
    static final String ANNOTATION_NAME = "@interface";//$NON-NLS-1$
    static final String METHOD_NAME = "method";        //$NON-NLS-1$

    private void createCoverageFilterControls(Composite parent) {
        final int initialMetricIndex = 0;
        final CovFilterCompareOperator initialCompOp = CovFilterCompareOperator.SMALLEREQUAL;
        final float initialCompareWith = 0.905f;

        this.viewerCovFilter = new ViewerCoverageFilter(initialMetricIndex, initialCompOp, initialCompareWith);

        this.bttCovFilterToggle = new Button(parent, SWT.CHECK);
        this.bttCovFilterToggle.setText(Messages.getString("CoverageView.0")); //$NON-NLS-1$
        this.bttCovFilterToggle.setSelection(false);
        this.bttCovFilterToggle.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                if(CoverageView.this.bttCovFilterToggle.getSelection()) {
                    CoverageView.this.viewer.addFilter(
                            CoverageView.this.viewerCovFilter);
                } else {
                    CoverageView.this.viewer.removeFilter(
                            CoverageView.this.viewerCovFilter);
                }
            }
        });

        this.cmbCovFilterMetric = new Combo(parent,
                SWT.DROP_DOWN | SWT.SINGLE | SWT.READ_ONLY);
        for(CoverageMetric metric : this.sortedCoverageMetrics) {
            this.cmbCovFilterMetric.add(metric.getName());
        }
        this.cmbCovFilterMetric.select(initialMetricIndex);
        this.cmbCovFilterMetric.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index = CoverageView.this.cmbCovFilterMetric.getSelectionIndex();
                if (index >= 0 && index < CoverageView.this.sortedCoverageMetrics.size()) {
                    CoverageView.this.viewerCovFilter.setMetricIndex(index);
                    List<ViewerFilter> filters = Arrays.asList(CoverageView.this.viewer.getFilters());
                    if (filters.contains(CoverageView.this.viewerCovFilter)) {
                        CoverageView.this.viewer.refresh();
                    }
                } else {
                    CoverageView.this.logger.error("Unknown metric to filter selected."); //$NON-NLS-1$
                }
            }
        });

        this.cmbCovFilterCompOp = new Combo(parent,
                SWT.DROP_DOWN | SWT.SINGLE | SWT.READ_ONLY);
        for(CovFilterCompareOperator compOp : CovFilterCompareOperator.values()) {
            this.cmbCovFilterCompOp.add(compOp.getText());
        }
        this.cmbCovFilterCompOp.select(initialCompOp.ordinal());
        this.cmbCovFilterCompOp.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                int index
                    = CoverageView.this.cmbCovFilterCompOp.getSelectionIndex();
                CovFilterCompareOperator[] compOps = CovFilterCompareOperator.values();
                if(index >= 0 && index < compOps.length) {
                   CoverageView.this.viewerCovFilter.setCompareOperator(
                           compOps[index]);
                   List<ViewerFilter> filters = Arrays.asList(
                           CoverageView.this.viewer.getFilters());
                   if(filters.contains(CoverageView.this.viewerCovFilter)) {
                       CoverageView.this.viewer.refresh();
                   }
                } else {
                   CoverageView.this.logger.error("Unknown compare operator for filtering selected."); //$NON-NLS-1$
                }
            }
        });

        this.txtCovFilterPercent = new Text(parent, SWT.SINGLE | SWT.BORDER);
        NumberFormat nf = NumberFormat.getNumberInstance();
        nf.setMaximumFractionDigits(1);
        this.txtCovFilterPercent.setText(nf.format(initialCompareWith*100));
        this.txtCovFilterPercent.setTextLimit(8);
        this.txtCovFilterPercent.addListener(SWT.Verify, new Listener() {
            @Override
			public void handleEvent(Event e) {
                DecimalFormatSymbols dfs = new DecimalFormatSymbols();
                String string = e.text;
                char[] chars = new char[string.length()];
                string.getChars(0, chars.length, chars, 0);
                for (int i = 0; i < chars.length; i++) {
                    if (!(('0' <= chars[i] && chars[i] <= '9')
                        || (chars[i] == dfs.getDecimalSeparator()))) {
                        e.doit = false;
                        return;
                    }
                }
            }
        });
        this.txtCovFilterPercent.addModifyListener(new ModifyListener() {
            @Override
			public void modifyText(ModifyEvent event) {
                NumberFormat nf = NumberFormat.getNumberInstance();
                Number input;
                try {
                    input = nf.parse(
                        CoverageView.this.txtCovFilterPercent.getText());
                } catch(ParseException e) {
                    input = null;
                }
                if(input != null) {
                    CoverageView.this.txtCovFilterPercent.setBackground(null);
                    CoverageView.this.viewerCovFilter.setCoverageToCompareWith(
                            input.floatValue()/100);
                    List<ViewerFilter> filters = Arrays.asList(
                            CoverageView.this.viewer.getFilters());
                    if(filters.contains(CoverageView.this.viewerCovFilter)) {
                        CoverageView.this.viewer.refresh();
                    }
                } else {
                    CoverageView.this.txtCovFilterPercent.setBackground(
                            new Color(null, 255, 192, 192));
                }
            }
        });

        this.lblCovFilterPercent = new Label(parent, SWT.NONE);
        this.lblCovFilterPercent.setText("%"); //$NON-NLS-1$
    }

    private void layoutCoverageFilterControls() {
        FormData formData = new FormData();
        formData.top = new FormAttachment(this.cmbCovFilterMetric, 0, SWT.CENTER);
        formData.left = new FormAttachment(0, 5);
        this.bttCovFilterToggle.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(0, 5);
        formData.left = new FormAttachment(this.bttCovFilterToggle, 5);
        this.cmbCovFilterMetric.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(this.cmbCovFilterMetric, 0, SWT.CENTER);
        formData.left = new FormAttachment(this.cmbCovFilterMetric, 5);
        this.cmbCovFilterCompOp.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(this.cmbCovFilterMetric, 0, SWT.CENTER);
        formData.left = new FormAttachment(this.cmbCovFilterCompOp, 5);
        this.txtCovFilterPercent.setLayoutData(formData);

        formData = new FormData();
        formData.top = new FormAttachment(this.cmbCovFilterMetric, 0, SWT.CENTER);
        formData.left = new FormAttachment(this.txtCovFilterPercent, 5);
        this.lblCovFilterPercent.setLayoutData(formData);
    }

    /**
     * An enum holding the various compare operators for coverage filtering
     * and their <code>String</code>-representations which are displayed to the
     * user (in a combo box).
     */
    private static enum CovFilterCompareOperator {
        /**
         * The smaller operator: <code>&lt;</code>
         */
        SMALLER     ("<"), //$NON-NLS-1$
        /**
         * The smaller or equal operator: <code>&lt;=</code>
         */
        SMALLEREQUAL("<="), //$NON-NLS-1$
        /**
         * The equals operator: <code>=</code>
         */
        EQUAL       ("="), //$NON-NLS-1$
        /**
         * The greater or equal operator: <code>&gt;=</code>
         */
        GREATEREQUAL(">="), //$NON-NLS-1$
        /**
         * The greater operator: <code>&gt;</code>
         */
        GREATER     (">"); //$NON-NLS-1$

        private final String text;

        private CovFilterCompareOperator(String text) {
            this.text = text;
        }

        /**
         * Gets the text associated with this compare operator.
         *
         * @return   the text associated with this compare operator
         */
        public String getText() {
            return this.text;
        }
    }
    /**
     * Filters by coverage results (accuracy: percentage of the results are
     * rounded to the first digit after the decimal point, i.e.
     * 5.5% == 5.54% == 5.46%).
     */
    private class ViewerCoverageFilter extends ViewerFilter {

        private int metricIndex;

        private CovFilterCompareOperator compareOp;

        /**
         * coverage to compare width
         */
        private float compareWith;

        private int precision;

        /**
         * Constructor
         *
         * @param metricIndex
         * @param compareOp
         * @param compareWith
         */
        public ViewerCoverageFilter(int metricIndex, CovFilterCompareOperator compareOp,
                float compareWith) {
            this.setNumberOfPercentageFractionDigits(1);
            this.setMetricIndex(metricIndex);
            this.setCompareOperator(compareOp);
            this.setCoverageToCompareWith(compareWith);
        }

        /**
         * Sets the index of the metric
         *
         * @param metricIndex the given index.
         */
        public void setMetricIndex(int metricIndex) {
            this.metricIndex = metricIndex;
        }

        /**
         * Set the operator to use to compare the coverage result.
         *
         * @param compareOp
         *            the given {@link CovFilterCompareOperator}
         */
        public void setCompareOperator(CovFilterCompareOperator compareOp) {
            this.compareOp = compareOp;
        }

        /**
         * Sets the coverage to compare with
         *
         * @param compareWith
         *            the coverage between <code>0.0</code> and
         *            <code>1.0</code>
         */
        public void setCoverageToCompareWith(float compareWith) {
            this.compareWith = ((float)Math.round(compareWith*this.precision))
                                /((float)this.precision);
        }

        /**
         * Sets the number of percentage fractions. digits
         *
         * @param fractionDigits
         *            the number of digits
         */
        public void setNumberOfPercentageFractionDigits(int fractionDigits) {
            if(fractionDigits < 0) {
                throw new IllegalArgumentException("Fraction digits must be positive (or 0)."); //$NON-NLS-1$
            }
            this.precision = (int)Math.pow(10, fractionDigits+2);
        }

        /**
         * (non-Javadoc)
         *
         * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
        public boolean select(Viewer viewer, Object parent, Object element) {
            if(element instanceof HierarchyLevel
                    && Type.typeOf((HierarchyLevel)element) == Type.METHOD) {
                return filterByCoverageResult((HierarchyLevel)element);
            } else if(element instanceof HierarchyLevel) {
                /*
                 * Only include this HierarchyLevel (which ain't a method) if it
                 * contains at least one method which is included.
                 */
                for(HierarchyLevel childMethod
                        : this.fetchMethods((HierarchyLevel)element)) {
                    if(filterByCoverageResult(childMethod)) {
                        return true;
                    }
                }
                return false;
            } else {
                return true;
            }
        }

        /**
         * Returns whether the given <code>HierarchyLevel</code> makes it
         * through this filter.
         *
         * @return  <code>true</code> if element is included in the filtered
         *          set, and <code>false</code> if excluded
         */
        private boolean filterByCoverageResult(HierarchyLevel hLev) {
            final CoverageResult result = CoverageView.this.calculatedCoverage(this.metricIndex, hLev);

            float coverage;
            if(result == null) {
                return true;
            }
            coverage = ((float) Math.round(CoverageView.evaluateCoverageResult(result) * this.precision))
                    / ((float) this.precision);

            if(       (this.compareOp == CovFilterCompareOperator.EQUAL
                    || this.compareOp == CovFilterCompareOperator.SMALLEREQUAL
                    || this.compareOp == CovFilterCompareOperator.GREATEREQUAL)
                    && coverage == this.compareWith) {
                return true;
            } else if((this.compareOp == CovFilterCompareOperator.SMALLER
                    || this.compareOp == CovFilterCompareOperator.SMALLEREQUAL)
                    && coverage < this.compareWith) {
                return true;
            } else if((this.compareOp == CovFilterCompareOperator.GREATER
                    || this.compareOp == CovFilterCompareOperator.GREATEREQUAL)
                    && coverage > this.compareWith) {
                return true;
            } else {
                return false;
            }
        }

        private List<HierarchyLevel> fetchMethods(HierarchyLevel parent) {
            FetchMethodsVisitor visitor = new FetchMethodsVisitor();
            parent.accept(visitor, null, null, null, null, null, null, null, null);
            return visitor.getMethods();
        }

        private class FetchMethodsVisitor implements HierarchyLevel.Visitor {
            private List<HierarchyLevel> methods;

            /**
             * Constructor.
             */
            public FetchMethodsVisitor() {
                this.methods = new ArrayList<HierarchyLevel>();
            }

            /**
             * Gets the list of {@link HierarchyLevel}s, that are methods.
             *
             * @return the list of methods.
             */
            public List<HierarchyLevel> getMethods() {
                return this.methods;
            }
            @Override
			public void visit(HierarchyLevel hierarchyLevel) {
                if(Type.typeOf(hierarchyLevel) == Type.METHOD) {
                    this.methods.add(hierarchyLevel);
                }
            }
        }

    }

    /*
     * methods to fill information into the views widgets and to react on change
     */

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
     */
    @Override
	public void testSessionContainerActivated(ActiveTSContainerInfo tscInfo) {
        this.setViewerInput(tscInfo);
    }

    /**
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(ActiveTSContainerInfo)
     */
    @Override
	public void testCasesActivated(ActiveTSContainerInfo tscInfo) {
        this.setViewerInput(tscInfo);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerRemoved(org.codecover.eclipse.tscmanager.TSContainerManager.TSContainerInfo)
     */
    @Override
	public void testSessionContainerRemoved(TSContainerInfo tscInfo) {
        this.removeExpandedElements(tscInfo);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerAdded(org.codecover.eclipse.tscmanager.TSContainerManager.TSContainerInfo,
     *      int)
     */
    @Override
	public void testSessionContainerAdded(TSContainerInfo tscInfo, int index) {}

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo, ChangeType, TestCase)
     */
    @Override
	public void testCaseChanged(ActiveTSContainerInfo tscInfo,
            ChangeType changeType, TestCase testCase) {}

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(ActiveTSContainerInfo, ChangeType, TestSession)
     */
    @Override
	public void testSessionChanged(ActiveTSContainerInfo tscInfo,
            ChangeType changeType, TestSession testSession) {}

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerChanged(ChangeType, ActiveTSContainerInfo)
     */
    @Override
	public void testSessionContainerChanged(ChangeType changeType,
            ActiveTSContainerInfo tscInfo) {}

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#synchronizedStateChanged(TSContainerInfo, boolean)
     */
    @Override
	public void synchronizedStateChanged(TSContainerInfo tscInfo,
            boolean isSynchronized) {}

    /*
     * methods and class for saving and restoring the the expanded elements of
     * the visualized code tree
     */

    private void saveExpandedElements(TSContainerInfo tscInfo) {
        Set<String> expElems = new HashSet<String>();
        synchronized(this.updateLock) {
            if(tscInfo != null) {
                for(Object expandedElement : this.viewer.getExpandedElements()){
                    // checking for HierarchyLevel just to be safe
                    if(expandedElement instanceof HierarchyLevel) {
                        expElems.add(((HierarchyLevel)expandedElement).getId());
                    }
                }
                this.expandedHLevIDs.put(tscInfo, expElems);
            }
        }
    }

    private void restoreExpandedElements(ActiveTSContainerInfo tscInfo) {
        Set<Object> expandedHLevs = new HashSet<Object>();
        FindIDVisitor findIDVisitor = new FindIDVisitor();
        Set<String> expHLevIDs;
        synchronized(this.updateLock) {
            expHLevIDs = this.expandedHLevIDs.get(tscInfo.getTSContainerInfo());
            if(expHLevIDs != null) {
                for(String hLevID : expHLevIDs) {
                    findIDVisitor.setID(hLevID);
                    tscInfo.getTestSessionContainer().getCode().accept(
                           findIDVisitor, null, null, null, null, null, null,
                           null, null);
                    if(findIDVisitor.getHierarchyLevel() != null) {
                        expandedHLevs.add(findIDVisitor.getHierarchyLevel());
                    }
                }
            }
            this.viewer.setExpandedElements(expandedHLevs.toArray());
        }
    }

    private void removeExpandedElements(TSContainerInfo tscInfo) {
        synchronized(this.updateLock) {
            this.expandedHLevIDs.remove(tscInfo);
        }
    }

    private class FindIDVisitor implements HierarchyLevel.Visitor {
        private String id = null;

        private HierarchyLevel hLev = null;

        /**
         * Sets the id
         *
         * @param id
         *            the id to set.
         */
        public void setID(String id) {
            this.id = id;
            this.hLev = null;
        }

        /**
         * Gets the {@link HierarchyLevel}
         *
         * @return the {@link HierarchyLevel}
         */
        public HierarchyLevel getHierarchyLevel() {
            return this.hLev;
        }

        /**
         * (non-Javadoc)
         *
         * @see org.codecover.model.mast.HierarchyLevel.Visitor#visit(org.codecover.model.mast.HierarchyLevel)
         */
        @Override
		public void visit(HierarchyLevel hierarchyLevel) {
            if (hierarchyLevel.getId().equals(this.id)) {
                this.hLev = hierarchyLevel;
            }
        }
    }

    /**
     * Passing the focus request to the viewer's control.
     */
    @Override
    public void setFocus() {
        this.viewer.getControl().setFocus();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
     */
    @Override
    public void saveState(IMemento memento) {
        super.saveState(memento);

        for (TreeColumn treeColumn : this.viewer.getTree().getColumns()) {
            IMemento mem = memento.createChild(TAG_TYPE);
            mem.putInteger(TAG_COLUMN_INDEX, this.viewer.getTree()
                                                   .indexOf(treeColumn));
            mem.putInteger(TAG_COLUMN_WIDTH, treeColumn.getWidth());
            mem.putString(TAG_COLUMN_NAME, treeColumn.getText());
        }
    }

}
