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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.preferences.PreferencePageRoot;
import org.codecover.eclipse.preferences.RGBWithBoundaries;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManagerListener;
import org.codecover.eclipse.views.controls.LegendControl;
import org.codecover.eclipse.views.controls.MatrixControl;
import org.codecover.eclipse.views.controls.LegendControl.ILegendContentProvider;
import org.codecover.eclipse.views.controls.MatrixControl.CorrelationInfo;
import org.codecover.eclipse.views.controls.MatrixControl.IMatrixContentProvider;
import org.codecover.eclipse.views.controls.MatrixControl.TestCaseInfo;
import org.codecover.metrics.Metric;
import org.codecover.metrics.MetricProvider;
import org.codecover.metrics.correlation.CorrelationMetric;
import org.codecover.metrics.correlation.CorrelationResult;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.criteria.Criterion;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ContentViewer;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * This class represents an eclipse view. It is used to calculate and display
 * the results of the available {@link CorrelationMetric}s.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CorrelationView.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CorrelationView extends ViewPart {

    private static final Logger logger = CodeCoverPlugin.getDefault()
            .getLogger();

    private static final String TAG_WEIGHTS_LENGTH = "WeightsLength"; //$NON-NLS-1$

    private static final String TAG_FILTER_TREE_ACTION = "filterTreeAction"; //$NON-NLS-1$

    private static final String TAG_SHOW_LEGEND_ACTION = "showLegendAction"; //$NON-NLS-1$

    private static final String TAG_AUTO_CALCULATE_ACTION = "autoCalculateAction"; //$NON-NLS-1$

    private static final String TAG_SELECTED = "Selected"; //$NON-NLS-1$

    private static final String TAG_SASH_WEIGHTS = "sashWeights"; //$NON-NLS-1$

    private static final String TAG_SASH_WEIGHT_PREFIX = "sashWeight"; //$NON-NLS-1$

    private static final String TAG_ALL_CORRELATIONS_ACTION = "allCorrelationsAction"; //$NON-NLS-1$

    private final Set<CorrelationMetric> correlationMetrics;

    private final List<IAction> correlationActions;

    private IAction allCorrelationsAction;

    private IAction autoCalculateAction;

    private IAction filterTreeAction;

    private MatrixControl matrixControl;

    private TreeViewer treeViewer;

    private IAction showLegendAction;

    private final Map<CorrelationMetric, CorrelationResult> resultMap;

    private final GraphElement rootChainElement;

    private final List<TestCase> activeTestCases;

    private final List<TestCase> lastUsedTestCases;

    private ScrolledComposite scrolledComposite;

    private final Set<CorrelationMetric> activeMetrics;

    private IMemento memento;

    private SashForm sashForm;
    
    private boolean calculationPending = false;
    
    private final Object lock = new Object();

    private final CorrelationMetricComparator correlationMetricComparator = new CorrelationMetricComparator();

    private final TestCaseComparator testCaseComparator = new TestCaseComparator();
    
    private static final String TREE_LABEL_FORMAT = Messages
            .getString("CorrelationView.TREE_LABEL_FORMAT"); //$NON-NLS-1$

    private static final String TOOLTIP_SHOW_LEGEND_ACTION = Messages
            .getString("CorrelationView.TOOLTIP_SHOW_LEGEND_ACTION"); //$NON-NLS-1$

    private static final String DESCRIPTION_CHOOSE_CORRELATION_ACTION = Messages
            .getString("CorrelationView.DESCRIPTION_CHOOSE_CORRELATION_ACTION"); //$NON-NLS-1$

    private static final String TOOLTIP_CHOOSE_CORRELATION_ACTION = Messages
            .getString("CorrelationView.TOOLTIP_CHOOSE_CORRELATION_ACTION"); //$NON-NLS-1$

    private static final String LABEL_CHOOSE_CORRELATION_ACTION = Messages
            .getString("CorrelationView.LABEL_CHOOSE_CORRELATION_ACTION"); //$NON-NLS-1$

    private static final String TOOLTIP_AUTO_CALCULATE_ACTION = Messages
            .getString("CorrelationView.TOOLTIP_AUTO_CALCULATE_ACTION"); //$NON-NLS-1$

    private static final String DESCRIPTION_AUTO_CALCULATE_ACTION = Messages
            .getString("CorrelationView.DESCRIPTION_AUTO_CALCULATE_ACTION"); //$NON-NLS-1$

    private static final String LABEL_AUTO_CALCULATE_ACTION = Messages
            .getString("CorrelationView.LABEL_AUTO_CALCULATE_ACTION"); //$NON-NLS-1$

    private static final String DESCRIPTION_SHOW_LEGEND_ACTION = Messages
            .getString("CorrelationView.DESCRIPTION_SHOW_LEGEND_ACTION"); //$NON-NLS-1$

    private static final String LABEL_SHOW_LEGEND_ACTION = Messages
            .getString("CorrelationView.LABEL_SHOW_LEGEND_ACTION"); //$NON-NLS-1$

    private static final String TOOLTIP_FILTER_TREE_ACTION = Messages
            .getString("CorrelationView.TOOLTIP_FILTER_TREE_ACTION"); //$NON-NLS-1$

    private static final String DESCRIPTION_FILTER_TREE_ACTION = Messages
            .getString("CorrelationView.DESCRIPTION_FILTER_TREE_ACTION"); //$NON-NLS-1$

    private static final String LABEL_FILTER_TREE_ACTION = Messages
            .getString("CorrelationView.LABEL_FILTER_TREE_ACTION"); //$NON-NLS-1$

    private static final String DESCRIPTION_ALL_CORRELATION_METRICS = Messages
            .getString("CorrelationView.DESCRIPTION_ALL_CORRELATION_METRICS"); //$NON-NLS-1$

    private static final String LABEL_ALL_CORRELATION_METRICS = Messages
            .getString("CorrelationView.LABEL_ALL_CORRELATION_METRICS"); //$NON-NLS-1$

    private static final String LABEL_CSV_EXPORT_FILE_DIALOG = Messages
            .getString("CorrelationView.LABEL_CSV_EXPORT_FILE_DIALOG"); //$NON-NLS-1$

    private static final String LABEL_CSV_EXPORT_ACTION = Messages
            .getString("CorrelationView.LABEL_CSV_EXPORT_ACTION"); //$NON-NLS-1$

    private static final String CSV_EXPORT_TOOLTIP_DESCRIPTION = Messages
            .getString("CorrelationView.CSV_EXPORT_TOOLTIP_DESCRIPTION"); //$NON-NLS-1$

    /**
     * Constructor. 
     */
    public CorrelationView() {
        this.resultMap = new HashMap<CorrelationMetric, CorrelationResult>();
        this.activeTestCases = new Vector<TestCase>();
        this.lastUsedTestCases = new Vector<TestCase>();
        this.activeMetrics = new TreeSet<CorrelationMetric>(
                this.correlationMetricComparator);
        this.correlationMetrics = new TreeSet<CorrelationMetric>(
                this.correlationMetricComparator);
        this.correlationActions = new Vector<IAction>();
        this.rootChainElement = new GraphElement();
        CodeCoverPlugin.getDefault().getTSContainerManager().addListener(
                new TSManagerListener());
    }

    /**
     * Gets all the {@link CorrelationMetric}s
     * 
     * @return the {@link Set} of {@link CorrelationMetric}s
     */
    private final static Set<CorrelationMetric> getCorrelationMetrics() {
        Set<CorrelationMetric> correlationMetrics = new HashSet<CorrelationMetric>();
        for (Metric metric : MetricProvider.getAvailabeMetrics(CodeCoverPlugin
                .getDefault().getEclipsePluginManager().getPluginManager(),
                CodeCoverPlugin.getDefault().getLogger())) {
            if (metric instanceof CorrelationMetric) {
                correlationMetrics.add((CorrelationMetric) metric);
            }
        }

        return correlationMetrics;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        this.correlationMetrics.addAll(getCorrelationMetrics());

        this.filterTreeAction = createActionFilterTree();
        this.showLegendAction = createActionShowLegend();
        this.autoCalculateAction = createActionAutoCalculate();

        this.correlationActions.addAll(createCorrelationActions());
        this.allCorrelationsAction = createAllCorrelationsAction();

        initializeToolBar();

        this.sashForm = new SashForm(parent, SWT.HORIZONTAL);

        // Initialize the tree viewer pane.
        Composite leftComposite = new Composite(this.sashForm, SWT.NONE);
        leftComposite.setLayout(new GridLayout(1, false));

        createTreeViewerPane(leftComposite);

        // Initialize the matrix pane.
        Composite rightComposite = new Composite(this.sashForm, SWT.NONE);
        rightComposite.setLayout(new GridLayout(2, false));

        createCorrelationMatrixPane(rightComposite);

        // Set the weights of the sash from.
        this.sashForm.setWeights(getWeightsFromMemento());

        calculateCorrelation();

        this.lastUsedTestCases.clear();
        this.activeTestCases.clear();
        ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        if (activeTSContainer != null) {
            this.lastUsedTestCases.addAll(sortTestCases(activeTSContainer
                    .getActiveTestCases()));
            this.activeTestCases.addAll(sortTestCases(activeTSContainer
                    .getActiveTestCases()));
        }

        calculateSumbsumptionChain();
        refreshTreeViewer();
        refreshMatrix();

        CodeCoverPlugin.getDefault().getPreferenceStore()
                .addPropertyChangeListener(new PreferenceChangeListener());

        this.treeViewer.setInput(this.rootChainElement);
    }

    private final int[] getWeightsFromMemento() {
        int[] weights = new int[] {1, 4};

        if (this.memento != null) {
            IMemento sashWeightsMemento = this.memento
                    .getChild(TAG_SASH_WEIGHTS);
            if (sashWeightsMemento != null) {
                int length = sashWeightsMemento.getInteger(TAG_WEIGHTS_LENGTH);
                weights = new int[length];
                for (int i = 0; i < length; i++) {
                    weights[i] = sashWeightsMemento
                            .getInteger(TAG_SASH_WEIGHT_PREFIX + i);
                }
            }
        }
        return weights;
    }

    /*
     * Creates the tree viewer pane on the given composite. @param mainComposite
     * the given composite.
     */
    private final void createTreeViewerPane(Composite mainComposite) {
        this.treeViewer = new TreeViewer(mainComposite, SWT.H_SCROLL
                | SWT.V_SCROLL);

        this.treeViewer.setContentProvider(new TreeContentProvider());
        this.treeViewer.setLabelProvider(new TreeLabelProvider());
        this.treeViewer.setComparator(new TreeComparator());

        this.treeViewer.getTree().setLayoutData(
                new GridData(SWT.FILL, SWT.FILL, true, true));
    }

    /**
     * Creates the matrix pane in the given composite
     * 
     * @param mainComposite
     *            the given composite.
     */
    private final void createCorrelationMatrixPane(Composite mainComposite) {
        this.scrolledComposite = new ScrolledComposite(mainComposite,
                SWT.H_SCROLL | SWT.V_SCROLL);
        this.matrixControl = new MatrixControl(this.scrolledComposite,
                SWT.BORDER);

        this.matrixControl
                .setMatrixContentProvider(new MatrixContentProvider());

        this.matrixControl
                .setLegendContentProvider(new LegendContentProvider());

        this.matrixControl.setShowLegend(this.showLegendAction.isChecked());

        this.scrolledComposite.setContent(this.matrixControl);
        this.scrolledComposite.setExpandVertical(true);
        this.scrolledComposite.setExpandHorizontal(true);
        this.scrolledComposite.setAlwaysShowScrollBars(true);
        this.scrolledComposite.addControlListener(new ControlAdapter() {
            @Override
            public void controlResized(ControlEvent e) {
                CorrelationView.this.scrolledComposite
                        .setMinSize(CorrelationView.this.matrixControl
                                .computeSize(SWT.DEFAULT, SWT.DEFAULT));
            }
        });

        this.scrolledComposite.setLayoutData(new GridData(SWT.FILL, SWT.FILL,
                true, true));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        this.matrixControl.forceFocus();
    }

    private final Color getColorForCorrelation(double correlation) {
        Display display = getViewSite().getShell().getDisplay();

        for (RGBWithBoundaries boundaries : PreferencePageRoot
                .getCorrelationMatrixColors()) {
            double lower = (double) boundaries.getLowerBoundary() / 100;
            double upper = (double) boundaries.getUpperBoundary() / 100;

            if (lower == upper) {
                if (correlation == lower) {
                    return new Color(display, boundaries.getRGB());
                }
            } else {
                if (correlation >= lower && correlation < upper) {
                    return new Color(display, boundaries.getRGB());
                }
            }
        }
        return display.getSystemColor(SWT.COLOR_BLACK);
    }

    private final List<CorrelationInfo> getCorrelationInfos() {
        List<CorrelationInfo> list = new Vector<CorrelationInfo>();

        if (this.activeMetrics.isEmpty()) {
            return list;
        }

        for (int i = 0; i < this.lastUsedTestCases.size(); i++) {
            TestCase firstTestCase = this.lastUsedTestCases.get(i);
            for (int a = 0; a < this.lastUsedTestCases.size(); a++) {
                TestCase secondTestCase = this.lastUsedTestCases.get(a);
                Set<TestCase> testCases = new HashSet<TestCase>();
                testCases.add(firstTestCase);
                testCases.add(secondTestCase);

                int amountFirstTestCase = 0;
                int amountSecondTestCase = 0;
                int amountShared = 0;

                // XXX: (see comment in getCummulatedCorrelation) -- Johannes
                double correlation = getCummulatedCorrelation(firstTestCase,
                        secondTestCase);

                for (CorrelationMetric correlationMetric : this.activeMetrics) {
                    CorrelationResult result = this.resultMap
                            .get(correlationMetric);

                    if (result == null) {
                        continue;
                    }

                    amountFirstTestCase += result
                            .getCoverableItemCount(firstTestCase);
                    amountSecondTestCase += result
                            .getCoverableItemCount(secondTestCase);
                    amountShared += result
                            .getSharedCoverableItemCount(testCases);
                }

                CorrelationInfo correlationInfo = this.matrixControl.new CorrelationInfo(
                        correlation, amountShared, amountFirstTestCase,
                        amountSecondTestCase);

                list.add(correlationInfo);
            }
        }

        return list;
    }

    private final List<TestCaseInfo> getTestCaseInfos() {
        List<TestCaseInfo> list = new Vector<TestCaseInfo>();

        if (this.activeMetrics.isEmpty()) {
            return list;
        }

        for (TestCase testCase : this.lastUsedTestCases) {
            TestCaseInfo testCaseInfo = this.matrixControl.new TestCaseInfo(
                    testCase.getName(), truncateString(testCase.getName(), 9),
                    testCase.getTestSession().getName());

            list.add(testCaseInfo);
        }

        return list;
    }

    private final String truncateString(String text, int length) {
        String trailString = "…"; //$NON-NLS-1$

        if (text.length() <= length) {
            return text;
        }

        String truncatedString = text.substring(0, length
                - trailString.length());
        truncatedString += trailString;

        return truncatedString;
    }

    private final void initializeToolBar() {
        IToolBarManager toolBarManager = getViewSite().getActionBars()
                .getToolBarManager();

        toolBarManager.add(createActionCSVExport());
        toolBarManager.add(this.filterTreeAction);
        toolBarManager.add(createActionChooseMetric());
        toolBarManager.add(this.autoCalculateAction);
        toolBarManager.add(this.showLegendAction);
    }

    private final IAction createActionFilterTree() {
        IAction filterTreeAction = new Action(LABEL_FILTER_TREE_ACTION,
                IAction.AS_CHECK_BOX) {
            @Override
            public void run() {
                onFilterTree();
            }
        };
        filterTreeAction.setChecked(getBooleanFromMemento(
                TAG_FILTER_TREE_ACTION, false));
        filterTreeAction.setDescription(DESCRIPTION_FILTER_TREE_ACTION);
        filterTreeAction.setToolTipText(TOOLTIP_FILTER_TREE_ACTION);
        filterTreeAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.HIDE_TREE_ITEMS.getPath()));

        return filterTreeAction;
    }

    private final IAction createActionShowLegend() {
        IAction showLegendAction = new Action(LABEL_SHOW_LEGEND_ACTION,
                IAction.AS_CHECK_BOX) {
            @Override
            public void run() {
                onShowLegend();
            }
        };
        showLegendAction.setChecked(getBooleanFromMemento(
                TAG_SHOW_LEGEND_ACTION, true));
        showLegendAction.setDescription(DESCRIPTION_SHOW_LEGEND_ACTION);
        showLegendAction.setToolTipText(TOOLTIP_SHOW_LEGEND_ACTION);
        showLegendAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.SHOW_LEGEND.getPath()));

        return showLegendAction;
    }

    private final IAction createActionAutoCalculate() {
        IAction autoCalculateAction = new Action(LABEL_AUTO_CALCULATE_ACTION,
                IAction.AS_CHECK_BOX) {
            @Override
            public void run() {
                if (CorrelationView.this.autoCalculateAction.isChecked()) {
                    onCalculateCorrelation();
                }
            }
        };
        autoCalculateAction.setChecked(getBooleanFromMemento(
                TAG_AUTO_CALCULATE_ACTION, true));
        autoCalculateAction.setDescription(DESCRIPTION_AUTO_CALCULATE_ACTION);
        autoCalculateAction.setToolTipText(TOOLTIP_AUTO_CALCULATE_ACTION);
        autoCalculateAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.AUTO_CALCULATE.getPath()));

        return autoCalculateAction;
    }

    private final IAction createActionChooseMetric() {
        IAction chooseMetricAction = new Action(
                LABEL_CHOOSE_CORRELATION_ACTION, IAction.AS_DROP_DOWN_MENU) {
            @Override
            public void run() {
                onCalculateCorrelation();
            }
        };
        chooseMetricAction
                .setDescription(DESCRIPTION_CHOOSE_CORRELATION_ACTION);
        chooseMetricAction.setToolTipText(TOOLTIP_CHOOSE_CORRELATION_ACTION);
        chooseMetricAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.CALCULATE_CORRELATION.getPath()));
        chooseMetricAction.setMenuCreator(new MetricMenuCreator());

        return chooseMetricAction;
    }

    private final IAction createActionCSVExport() {
        IAction exportCSVAction = new Action(LABEL_CSV_EXPORT_ACTION,
                IAction.AS_PUSH_BUTTON) {
            @Override
            public void run() {
                onExportCSV();
            }
        };
        exportCSVAction.setDescription(CSV_EXPORT_TOOLTIP_DESCRIPTION);
        exportCSVAction.setToolTipText(CSV_EXPORT_TOOLTIP_DESCRIPTION);
        exportCSVAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.CSV_EXPORT.getPath()));

        return exportCSVAction;
    }

    private final void refreshLastUsedTestCases(Collection<TestCase> testCases) {
        this.lastUsedTestCases.clear();
        this.lastUsedTestCases.addAll(sortTestCases(testCases));
    }

    private final void onActiveTestCasesChanged(Set<TestCase> testCases) {
        refreshActiveTestCaseList(testCases);

        if (this.autoCalculateAction.isChecked()) {
            refreshLastUsedTestCases(this.activeTestCases);
            refreshTreeViewer();
            refreshMatrix();
        }
    }

    private final void onTSCChanged(Set<TestCase> testCases) {
        synchronized (lock) {
            this.calculationPending = false;
        }

        refreshActiveTestCaseList(testCases);

        if (this.autoCalculateAction.isChecked()) {
            onCalculateCorrelation();
            
        }
    }

    private final void onExportCSV() {
        FileDialog fileDialog = new FileDialog(getSite().getShell(), SWT.SAVE);
        fileDialog.setText(LABEL_CSV_EXPORT_FILE_DIALOG);
        fileDialog.setFilterExtensions(new String[] {"*.csv", "*.*"}); //$NON-NLS-1$//$NON-NLS-2$

        String location = fileDialog.open();

        if (location != null) {
            File exportFile = new File(location);
            String outputString = this.matrixControl.createCSVExportString();

            exportFile.delete();

            FileOutputStream fileOutputStream = null;

            try {
                fileOutputStream = new FileOutputStream(exportFile, true);

                FileChannel outChannel = fileOutputStream.getChannel();
                ByteBuffer buffer = ByteBuffer.allocate(outputString.length());

                byte[] bytes = outputString.getBytes();
                buffer.put(bytes);
                buffer.flip();

                outChannel.write(buffer);
                fileOutputStream.close();
            } catch (FileNotFoundException e) {
                CorrelationView.logger.error("A FileNotFoundException " + //$NON-NLS-1$
                        "occurred during csv export", e); //$NON-NLS-1$
            } catch (IOException e) {
                CorrelationView.logger.error(
                        "A IOException occurred during csv export", e); //$NON-NLS-1$
            }
        }
    }

    private final void onShowLegend() {
        this.matrixControl.setShowLegend(!this.matrixControl.isShowLegend());
        refreshMatrix();
    }

    private final void onChooseMetric(CorrelationMetric correlationMetric) {
        this.activeMetrics.clear();
        this.activeMetrics.add(correlationMetric);

        refreshTreeViewer();
        refreshMatrix();
    }

    private final void onChooseAllMetrics() {
        this.activeMetrics.addAll(this.correlationMetrics);

        refreshTreeViewer();
        refreshMatrix();
    }

    private final void onFilterTree() {
        boolean visible = !this.filterTreeAction.isChecked();
        for (GraphElement graphElement : this.rootChainElement.children) {
            if (graphElement.children.isEmpty()) {
                graphElement.visible = visible;
            } else {
                graphElement.visible = true;
            }
        }

        refreshTreeViewer();
    }

    private final void onCalculateCorrelation() {
        calculateCorrelation();
        calculateSumbsumptionChain();
        refreshTreeViewer();
        refreshMatrix();
    }
    
    private final List<TestCase> getAllTestCasesFromActiveContainer() {
        List<TestCase> testCases = new LinkedList<TestCase>();
        
        ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        if (activeTSContainer == null) {
            return testCases;
        }

        TestSessionContainer testSessionContainer = activeTSContainer
                .getTestSessionContainer();


        for (TestSession testSession : testSessionContainer.getTestSessions()) {
            testCases.addAll(testSession.getTestCases());
        }
        
        return testCases;
    }

    private final void calculateCorrelation() {
        ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        if (activeTSContainer == null) {
            return;
        }

        TestSessionContainer testSessionContainer = activeTSContainer
                .getTestSessionContainer();

        List<TestCase> testCases = new Vector<TestCase>();

        for (TestSession testSession : testSessionContainer.getTestSessions()) {
            testCases.addAll(testSession.getTestCases());
        }

        // If there are no test cases, then nothing can be calculated.
        if (testCases.isEmpty()) {
            return;
        }

        Set<TestCase> allTestCases = new HashSet<TestCase>(testCases);
        Set<TestCase> lastUsed = new HashSet<TestCase>(this.activeTestCases);

        /*
         * We remove all those test cases, that were still in the collection of
         * last used test cases, but not in the total list of test cases of the
         * current test session container. This will fix the synchronization
         * between the TSContainerManager and this view, in case of a change in
         * the test session container, e.g. a deletion.
         */
        lastUsed.retainAll(allTestCases);

        refreshLastUsedTestCases(lastUsed);

        this.resultMap.clear();

        Set<Criterion> availableCriteria = testSessionContainer.getCriteria();

        for (CorrelationMetric correlationMetric : this.correlationMetrics) {
            // Only calculate those metrics, whose required criteria are
            // present.
            if (availableCriteria.containsAll(correlationMetric
                    .getRequiredCriteria())) {

                CorrelationResult result = correlationMetric
                        .calculateCorrelation(testCases);

                this.resultMap.put(correlationMetric, result);
            }
        }
    }

    private final boolean isTestCaseActive(TestCase testCase) {
        return this.lastUsedTestCases.contains(testCase);
    }

    private final void calculateSumbsumptionChain() {
        List<GraphElement> elements = new Vector<GraphElement>();

        this.rootChainElement.reset();

        if (this.activeMetrics.size() == 0) {
            return;
        }
        
        List<TestCase> testCases = new LinkedList<TestCase>(
                getAllTestCasesFromActiveContainer());

        /* If there are no test cases, then nothing can be calculated. */
        if (testCases.isEmpty()) {
            return;
        }
        
        for (TestCase testCase : testCases) {
            GraphElement element = new GraphElement();
            element.testCases.add(testCase);

            elements.add(element);
        }

        for (GraphElement topElement : elements) {
            boolean noChange = false;

            while (!noChange) {
                noChange = true;
                for (TestCase testCase : testCases) {

                    double correlation = getCummulatedCorrelation(
                            new Vector<TestCase>(topElement.testCases)
                                    .firstElement(), testCase);

                    if (correlation != 1.0) {
                        continue;
                    } else {
                        noChange &= !recurseGraphElement(topElement, testCase);
                    }
                }
            }
        }

        /* Add all elements, which have no parent to the root element. */
        for (GraphElement graphElement : elements) {
            GraphElement subElement = graphElement;
            while (subElement.parent != null) {
                subElement = subElement.parent;
            }

            subElement.parent = this.rootChainElement;
            this.rootChainElement.children.add(subElement);

            // Hide the top level elements, if the action is set.
            if (subElement.children.isEmpty()) {
                subElement.visible = !this.filterTreeAction.isChecked();
            }
        }
    }

    private final boolean recurseGraphElement(GraphElement element,
            TestCase testCase) {
        boolean anyInsertion = false;
        Iterator<GraphElement> iterator = new HashSet<GraphElement>(
                element.children).iterator();
        while (iterator.hasNext()) {
            anyInsertion |= recurseGraphElement(iterator.next(), testCase);
        }

        if (!anyInsertion) {
            double topCorrelation = getCummulatedCorrelation(
                    new Vector<TestCase>(element.testCases).firstElement(),
                    testCase);
            double bottomCorrelation = getCummulatedCorrelation(testCase,
                    new Vector<TestCase>(element.testCases).firstElement());

            if (topCorrelation == 1.0 && bottomCorrelation == 1.0) {
                anyInsertion = element.testCases.add(testCase);
            } else if (topCorrelation == 1.0) {
                GraphElement newElement = new GraphElement();
                newElement.testCases.add(testCase);
                newElement.parent = element;

                boolean alreadyPresentInTree = false;
                for (GraphElement child : element.children) {
                    double childCorrelation = getCummulatedCorrelation(
                            new Vector<TestCase>(child.testCases)
                                    .firstElement(), testCase);
                    if (childCorrelation == 1.0) {
                        alreadyPresentInTree = true;
                        break;
                    }
                }

                if (!alreadyPresentInTree) {
                    anyInsertion = element.children.add(newElement);
                }
            } else if (bottomCorrelation == 1.0 && element.parent != null) {
                GraphElement newElement = new GraphElement();
                newElement.testCases.add(testCase);

                GraphElement parent = element.parent;

                double parentTopCorrelation = getCummulatedCorrelation(
                        new Vector<TestCase>(parent.testCases).firstElement(),
                        testCase);
                double parentBottonCorrelation = getCummulatedCorrelation(
                        testCase, new Vector<TestCase>(parent.testCases)
                                .firstElement());
                if (parentTopCorrelation == 1.0
                        && parentBottonCorrelation == 1.0) {
                    anyInsertion = false;
                } else if (parentTopCorrelation == 1.0) {

                    parent.children.remove(element);
                    parent.children.add(newElement);

                    newElement.parent = parent;
                    newElement.children.add(element);

                    element.parent = newElement;
                    anyInsertion = true;
                }
            }
        }

        return anyInsertion;
    }

    // XXX: averaging is bad here, because Metrics without coverable items make
    // the result diverge from what you expect, but they shouldn't change the
    // result at all. Rethink this whole thing and make it's own Metric? --
    // Johannes
    private final double getCummulatedCorrelation(TestCase case1, TestCase case2) {
        double correlation = 0.0;
        int summands = 0;

        for (CorrelationMetric correlationMetric : this.activeMetrics) {
            CorrelationResult result = this.resultMap.get(correlationMetric);

            if (result == null) {
                continue;
            }

            if (result.getCoverableItemCount(case2) > 0) {

                /*
                 * this metric is relevant, because it detected coverable items
                 * in the second test case
                 */
                correlation += result.getCorrelation(case1, case2);
                ++summands;
            }
        }

        // We have added the correlations for each relevant metric together, so
        // now we have to divide it by the amount of summands.
        if (summands > 0) {
            correlation /= summands;
        } else {
            /* no metric is contributing coverable items */
            correlation = 1.0;
        }

        return correlation;
    }

    private final List<IAction> createCorrelationActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> correlationActions = new Vector<IAction>();

        for (final CorrelationMetric correlationMetric : this.correlationMetrics) {
            String name = correlationMetric.getName();
            IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseMetric(correlationMetric);
                }
            };
            action.setDescription(correlationMetric.getDescription());
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(getBooleanFromMemento(escapeWhiteSpaces(name),
                    false));

            if (action.isChecked()) {
                // Add the metric to the activeMetrics set, if the action is
                // checked.
                this.activeMetrics.add(correlationMetric);
            }

            correlationActions.add(action);
        }

        return correlationActions;
    }

    private final IAction createAllCorrelationsAction() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        IAction action = new Action(LABEL_ALL_CORRELATION_METRICS,
                IAction.AS_RADIO_BUTTON) {
            @Override
            public void run() {
                onChooseAllMetrics();
            }
        };
        action.setDescription(DESCRIPTION_ALL_CORRELATION_METRICS);
        action.setImageDescriptor(platformImages
                .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

        // We check the all correlations action, if no other actions were
        // selected.
        action.setChecked(getBooleanFromMemento(TAG_ALL_CORRELATIONS_ACTION,
                areNoDistinctCorrelationActionChecked()));

        if (action.isChecked()) {
            this.activeMetrics.addAll(this.correlationMetrics);
        }

        return action;
    }

    private final boolean areNoDistinctCorrelationActionChecked() {
        for (IAction correlationAction : this.correlationActions) {
            if (correlationAction.isChecked()) {
                return false;
            }
        }

        return true;
    }

    private final boolean getBooleanFromMemento(String tagName,
            boolean defaultValue) {
        boolean checked = defaultValue;
        if (this.memento != null) {
            IMemento mem = this.memento.getChild(tagName);
            if (mem != null) {
                checked = Boolean.parseBoolean(mem.getString(TAG_SELECTED));
            }
        }
        return checked;
    }

    private final void refreshMatrix() {
        this.matrixControl.refreshMatrix();

        this.scrolledComposite.setMinSize(this.matrixControl.computeSize(
                SWT.DEFAULT, SWT.DEFAULT));
    }

    private final void refreshTreeViewer() {
        this.treeViewer.refresh();
    }

    private final void refreshActiveTestCaseList(Set<TestCase> testCases) {
        this.activeTestCases.clear();
        this.activeTestCases.addAll(sortTestCases(testCases));
    }

    private final List<TestCase> sortTestCases(Collection<TestCase> testCases) {
        List<TestCase> list = new Vector<TestCase>();
        list.addAll(testCases);
        Collections.sort(list, this.testCaseComparator);

        return list;
    }

    /*
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
     */
    @Override
    public void saveState(IMemento memento) {
        super.saveState(memento);

        for (IAction action : this.correlationActions) {
            IMemento mem = memento.createChild(escapeWhiteSpaces(action
                    .getText()));

            mem.putString(TAG_SELECTED, Boolean.toString(action.isChecked()));
        }
        IMemento allCorrelationsMemento = memento
                .createChild(TAG_ALL_CORRELATIONS_ACTION);
        allCorrelationsMemento.putString(TAG_SELECTED, Boolean
                .toString(this.allCorrelationsAction.isChecked()));

        IMemento autoCalculateMemento = memento
                .createChild(TAG_AUTO_CALCULATE_ACTION);
        autoCalculateMemento.putString(TAG_SELECTED, Boolean
                .toString(this.autoCalculateAction.isChecked()));

        IMemento showLegendMemento = memento
                .createChild(TAG_SHOW_LEGEND_ACTION);
        showLegendMemento.putString(TAG_SELECTED, Boolean
                .toString(this.showLegendAction.isChecked()));

        IMemento hideTopLevelMemento = memento
                .createChild(TAG_FILTER_TREE_ACTION);
        hideTopLevelMemento.putString(TAG_SELECTED, Boolean
                .toString(this.filterTreeAction.isChecked()));

        IMemento sashWeightsMemento = memento.createChild(TAG_SASH_WEIGHTS);
        int[] weights = this.sashForm.getWeights();
        sashWeightsMemento.putInteger(TAG_WEIGHTS_LENGTH, weights.length);

        for (int i = 0; i < weights.length; i++) {
            int weight = weights[i];
            sashWeightsMemento.putInteger(TAG_SASH_WEIGHT_PREFIX + i, weight);
        }
    }

    private final String escapeWhiteSpaces(String text) {
        return text.replace(' ', '_');
    }

    private Runnable getTSCChangedRunnable(final ActiveTSContainerInfo tscInfo) {
        return new Runnable() {
            @Override
			public void run() {
                if (CorrelationView.this.matrixControl.isDisposed()) {
                    return;
                }
                if (tscInfo != null) {
                    onTSCChanged(tscInfo.getActiveTestCases());
                } else {
                    onTSCChanged(new HashSet<TestCase>());
                }
            }
        };
    }

    private Runnable getActiveTestCasesChangedRunnable(
            final ActiveTSContainerInfo tscInfo) {
        return new Runnable() {
            @Override
			public void run() {
                if (CorrelationView.this.matrixControl.isDisposed()) {
                    return;
                }
                if (tscInfo != null) {
                    onActiveTestCasesChanged(tscInfo.getActiveTestCases());
                } else {
                    onActiveTestCasesChanged(new HashSet<TestCase>());
                }
            }
        };
    }

    private final class TestCaseComparator implements Comparator<TestCase> {
        /**
         * Compares two {@link TestCase}s
         * 
         * @param testCase1
         *            the first {@link TestCase}
         * @param testCase2
         *            the second {@link TestCase}
         * @return the comparison result of the names of the {@link TestCase}'s
         *         {@link TestSession}s, or, if those names are equal, the
         *         comparison result of the names of the {@link TestCase}s
         */
        @Override
		public int compare(TestCase testCase1, TestCase testCase2) {
            int result;
            result = testCase1.getTestSession().getName().compareTo(
                    testCase2.getTestSession().getName());

            if (result == 0) {
                result = testCase1.getName().compareTo(testCase2.getName());
            }

            return result;
        }
    }

    private final class GraphElement {
        GraphElement parent;

        boolean visible = true;

        final Set<TestCase> testCases = new TreeSet<TestCase>(
                CorrelationView.this.testCaseComparator);

        final Set<GraphElement> children = new HashSet<GraphElement>();

        /**
         * Resets the data of the element.
         */
        void reset() {
            this.parent = null;
            this.visible = true;
            this.testCases.clear();
            this.children.clear();
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#equals(java.lang.Object)
         */
        @Override
        public boolean equals(Object obj) {
            if (!(obj instanceof GraphElement)) {
                return false;
            }

            GraphElement that = (GraphElement) obj;

            if (!this.testCases.equals(that.testCases)) {
                return false;
            }

            return true;
        }

        /*
         * (non-Javadoc)
         * 
         * @see java.lang.Object#hashCode()
         */
        @Override
        public int hashCode() {
            int hash = 0;

            for (TestCase testCase : this.testCases) {
                hash += testCase.getName().hashCode();
                hash += testCase.getTestSession().getName().hashCode();
            }

            return hash;
        }
    }

    private final class MetricMenuCreator implements IMenuCreator {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.IMenuCreator#dispose()
         */
        @Override
		public void dispose() {
            // Nothing to do here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Control)
         */
        @Override
		public Menu getMenu(Control parent) {
            Menu dropDownMenu = new Menu(parent);
            for (IAction action : CorrelationView.this.correlationActions) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

                contributionItem.fill(dropDownMenu, -1);

            }

            // new MenuItem(dropDownMenu, SWT.SEPARATOR);

            ActionContributionItem allMetricsItem = new ActionContributionItem(
                    CorrelationView.this.allCorrelationsAction);

            allMetricsItem.fill(dropDownMenu, -1);

            return dropDownMenu;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.action.IMenuCreator#getMenu(org.eclipse.swt.widgets.Menu)
         */
        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }
    }

    private final class MatrixContentProvider implements IMatrixContentProvider {
        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.MatrixControl.IMatrixContentProvider#getColorForCorrelation(double)
         */
        @Override
		public Color getColorForCorrelation(double correlation) {
            return CorrelationView.this.getColorForCorrelation(correlation);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.MatrixControl.IMatrixContentProvider#getCorrelationInfos()
         */
        @Override
		public List<CorrelationInfo> getCorrelationInfos() {
            return CorrelationView.this.getCorrelationInfos();
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.MatrixControl.IMatrixContentProvider#getTestCaseInfos()
         */
        @Override
		public List<TestCaseInfo> getTestCaseInfos() {
            return CorrelationView.this.getTestCaseInfos();
        }
    }

    private final class LegendContentProvider implements ILegendContentProvider {
        private LegendControl legendControl;

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.LegendControl.ILegendContentProvider#setClient(org.codecover.eclipse.views.controls.LegendControl)
         */
        @Override
		public void setClient(LegendControl legendControl) {
            this.legendControl = legendControl;

        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.LegendControl.ILegendContentProvider#getColors()
         */
        @Override
		public List<Color> getColors() {
            List<Color> list = new Vector<Color>();

            if (this.legendControl == null) {
                return list;
            }

            Display display = this.legendControl.getDisplay();

            for (RGBWithBoundaries boundaries : PreferencePageRoot
                    .getCorrelationMatrixColors()) {
                list.add(new Color(display, boundaries.getRGB()));
            }

            return list;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.views.controls.LegendControl.ILegendContentProvider#getLabels()
         */
        @Override
		public List<String> getLabels() {
            List<String> list = new Vector<String>();

            if (this.legendControl == null) {
                return list;
            }

            for (RGBWithBoundaries boundaries : PreferencePageRoot
                    .getCorrelationMatrixColors()) {
                int lower = boundaries.getLowerBoundary();
                int upper = boundaries.getUpperBoundary();

                StringBuilder sb = new StringBuilder();

                if (lower == upper) {
                    sb.append(String.format("%1$d%%", lower)); //$NON-NLS-1$
                } else {
                    sb.append(String.format("%1$d%% - %2$d%%", lower, upper)); //$NON-NLS-1$
                }

                list.add(sb.toString());
            }

            return list;
        }
    }

    private final class TreeContentProvider implements ITreeContentProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getChildren(java.lang.Object)
         */
        @Override
		public Object[] getChildren(Object parentElement) {

            if (parentElement instanceof GraphElement) {
                List<GraphElement> list = new Vector<GraphElement>();
                GraphElement element = (GraphElement) parentElement;

                for (GraphElement testCaseChainElement : element.children) {
                    list.addAll(rec(testCaseChainElement));
                }
                return removeDuplicateEntries(list).toArray();
            }

            return null;
        }

        private final List<GraphElement> rec(GraphElement graphElement) {
            List<GraphElement> list = new LinkedList<GraphElement>();

            boolean anySelection = false;
            for (TestCase testCase : graphElement.testCases) {
                anySelection |= isTestCaseActive(testCase);
            }

            if (anySelection && graphElement.visible) {
                list.add(graphElement);
            } else {
                for (GraphElement element : graphElement.children) {
                    list.addAll(rec(element));
                }
            }
            return list;
        }
        
        private final List<GraphElement> removeDuplicateEntries(
                List<GraphElement> inputList) {
            List<GraphElement> newList = new LinkedList<GraphElement>();

            for (GraphElement graphElement : inputList) {
                boolean alreadyPresent = false;
                for (GraphElement element : newList) {
                    if (element.testCases.equals(graphElement.testCases)) {
                        alreadyPresent = true;
                        break;
                    }
                }

                if (!alreadyPresent) {
                    newList.add(graphElement);
                }
            }
            
            return newList;
        }
        
        
        
        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#getParent(java.lang.Object)
         */
        @Override
		public Object getParent(Object element) {
            if (element instanceof GraphElement) {
                return ((GraphElement) element).parent;
            }
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITreeContentProvider#hasChildren(java.lang.Object)
         */
        @Override
		public boolean hasChildren(Object element) {
            if (element instanceof GraphElement) {
                if (((GraphElement) element).children.isEmpty()) {
                    return false;
                } else {
                    List<GraphElement> list = new LinkedList<GraphElement>();
                    for (GraphElement graphElement : ((GraphElement) element).children) {
                        return list.addAll(rec(graphElement));
                    }
                    return !list.isEmpty();
                }
            }
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        @Override
		public Object[] getElements(Object inputElement) {
            return getChildren(inputElement);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        @Override
		public void dispose() {
            // Do nothing here
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // The input will never be changed.
        }
    }

    private final class TreeLabelProvider implements ILabelProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ILabelProvider#getImage(java.lang.Object)
         */
        @Override
		public Image getImage(Object element) {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ILabelProvider#getText(java.lang.Object)
         */
        @Override
		public String getText(Object element) {
            if (element instanceof GraphElement) {
                StringBuilder sb = new StringBuilder();
                boolean firstElement = true;
                for (TestCase testCase : ((GraphElement) element).testCases) {
                    /* If a test case isn't active, no name is displayed. */
                    if (!isTestCaseActive(testCase)) {
                        continue;
                    }

                    String name = testCase.getName();
                    String sessionName = testCase.getTestSession().getName();
                    if (firstElement) {
                        firstElement = false;
                    } else {
                        sb.append(", "); //$NON-NLS-1$
                    }
                    sb.append(String.format(TREE_LABEL_FORMAT, name,
                            sessionName));
                }

                return sb.toString();
            }
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
         */
        @Override
		public void addListener(ILabelProviderListener listener) {
            // No Listener required.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
         */
        @Override
		public void dispose() {
            // Nothing needs to be done here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
         *      java.lang.String)
         */
        @Override
		public boolean isLabelProperty(Object element, String property) {
            // We don't need this feature.
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
         */
        @Override
		public void removeListener(ILabelProviderListener listener) {
            // No listeners are used in this provider
        }
    }

    private final class TreeComparator extends ViewerComparator {
        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ViewerComparator#compare(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
        public int compare(Viewer viewer, Object e1, Object e2) {
            IBaseLabelProvider baseLabelProvider = ((ContentViewer) viewer)
                    .getLabelProvider();
            if (baseLabelProvider instanceof ILabelProvider) {
                ILabelProvider labelProvider = (ILabelProvider) baseLabelProvider;
                return labelProvider.getText(e1).compareTo(
                        labelProvider.getText(e2));
            }

            return super.compare(viewer, e1, e2);
        }
    }

    private final class TSManagerListener implements TSContainerManagerListener {

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo,
         *      ChangeType, TestCase)
         */
        @Override
		public void testCaseChanged(final ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestCase testCase) {
            Display d = getSite().getShell().getDisplay();
            switch (changeType) {
                case CHANGE:
                    synchronized (lock) {
                        if (CorrelationView.this.calculationPending) {
                            return;
                        }

                        d.asyncExec(getActiveTestCasesChangedRunnable(tscInfo));
                    }
                    break;
                case ADD:
                    break;
                case REMOVE:
                    break;
                default:
                    return;
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(ActiveTSContainerInfo)
         */
        @Override
		public void testCasesActivated(final ActiveTSContainerInfo tscInfo) {
            Display disp = getSite().getShell().getDisplay();
            synchronized (lock) {
                if (CorrelationView.this.calculationPending) {
                    return;
                }

                disp.asyncExec(getActiveTestCasesChangedRunnable(tscInfo));
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(ActiveTSContainerInfo,
         *      ChangeType, TestSession)
         */
        @Override
		public void testSessionChanged(final ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestSession testSession) {
            Display disp = getSite().getShell().getDisplay();

            synchronized (lock) {
                if (CorrelationView.this.calculationPending) {
                    return;
                }

                CorrelationView.this.calculationPending = true;

                disp.asyncExec(getTSCChangedRunnable(tscInfo));
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
         */
        @Override
		public void testSessionContainerActivated(
                final ActiveTSContainerInfo tscInfo) {
            Display disp = getSite().getShell().getDisplay();
            
            synchronized (lock) {
                if (CorrelationView.this.calculationPending) {
                    return;
                }

                CorrelationView.this.calculationPending = true;

                disp.asyncExec(getTSCChangedRunnable(tscInfo));
            }
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
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerChanged(ChangeType,
         *      ActiveTSContainerInfo)
         */
        @Override
		public void testSessionContainerChanged(ChangeType changeType,
                final ActiveTSContainerInfo tscInfo) {
            Display disp = getSite().getShell().getDisplay();

            synchronized (lock) {
                if (CorrelationView.this.calculationPending) {
                    return;
                }

                CorrelationView.this.calculationPending = true;

                disp.asyncExec(getTSCChangedRunnable(tscInfo));
            }
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

    private final class PreferenceChangeListener implements
            IPropertyChangeListener {

        /*
         * * (non-Javadoc)
         * 
         * @see org.eclipse.jface.util.IPropertyChangeListener#propertyChange(org.eclipse.jface.util.PropertyChangeEvent)
         */
        @Override
		public void propertyChange(PropertyChangeEvent event) {
            CorrelationView.this.refreshMatrix();
        }
    }

    private final class CorrelationMetricComparator implements
            Comparator<CorrelationMetric> {

        /**
         * Compares two {@link CorrelationMetric}s according to their names.
         * 
         * @param o1
         *            the first {@link CorrelationMetric}
         * @param o2
         *            the second {@link CorrelationMetric}
         * @return the result of the comparison of their names.
         */
        @Override
		public int compare(CorrelationMetric o1, CorrelationMetric o2) {
            int result = 0;

            result = o1.getName().compareTo(o2.getName());

            return result;
        }
    }
}