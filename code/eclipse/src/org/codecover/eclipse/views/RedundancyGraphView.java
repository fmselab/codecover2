/******************************************************************************
 * Copyright (c) 2009 Negar Koochakzadeh, Vahid Garousi			      *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.views;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.apache.commons.collections15.Transformer;
import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManagerListener;
import org.codecover.eclipse.views.RedundancyGraphView.TestCasePair;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.utils.ChangeType;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
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

import edu.uci.ics.jung.algorithms.layout.ISOMLayout;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.SparseMultigraph;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ScalingControl;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import edu.uci.ics.jung.visualization.swt.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.swt.VisualizationComposite;

/**
 * This {@link RedundancyGraphView} is a view in eclipse. It provides the user with the opportunity to draw
 * the redundancy graph which shows how test cases are related with each other in terms of common covered
 * parts. Project supervisor: Vahid Garousi (http://www.ucalgary.ca/~vgarousi/) Software Quality Engineering
 * Research Group (SoftQual) Department of Electrical and Computer Engineering Schulich School of Engineering
 * University of Calgary, Alberta, Canada http://www.softqual.ucalgary.ca
 *
 * @author Negar Koochakzadeh
 * @version 1.0
 */
public class RedundancyGraphView
  extends ViewPart {

  private final List<IAction> correlationActions;

  private final List<IAction> TestLevels;

  private final List<IAction> MouseStyle;

  private final Map<TestCasePair, Double> PairRedundancy;

  private final Map<String, Double> SuiteRedundancy;

  private final List<TestCase> selectedTestCases = new Vector<TestCase>();

  private final List<TestCase> wholeTestCases = new Vector<TestCase>();

  private final List<TestCase> RedundantTestCases = new Vector<TestCase>();

  private SashForm sashForm;

  // private Graph redundancyGraph;
  private boolean calculationPending = false;

  private final Object lock = new Object();

  private String ActiveCriterion;

  private String SelectedMouseStyle;

  private GraphComposite graphComposite;

  private final TestCaseComparator testCaseComparator = new TestCaseComparator();

  private static final String LABEL_CHOOSE_COVERAGE_CRITERIA =
    Messages.getString("CoverageGraphView.COVERAGE_CRITERIA"); //$NON-NLS-1$

  private static final String LABEL_CHOOSE_TEST_LEVEL = Messages.getString("CoverageGraphView.TEST_LEVEL"); //$NON-NLS-1$

  private static final String LABEL_CSV_EXPORT_ACTION =
    Messages.getString("CorrelationView.LABEL_CSV_EXPORT_ACTION"); //$NON-NLS-1$

  private static final String LABEL_DRAW_GRAPH_ACTION =
    Messages.getString("CorrelationView.LABEL_DRAW_GRPAH_ACTION"); //$NON-NLS-1$

  private static final String LABEL_CHOOSE_MOUSE_STYLE = Messages.getString("CoverageGraphView.MOUSE_STYLE"); //$NON-NLS-1$

  /**
   * Constructor.
   */
  public RedundancyGraphView() {
    this.SelectedMouseStyle = "Picking";
    this.ActiveCriterion = "Statement";
    this.correlationActions = new Vector<IAction>();
    this.TestLevels = new Vector<IAction>();
    this.MouseStyle = new Vector<IAction>();
    this.PairRedundancy = new HashMap<TestCasePair, Double>();
    this.SuiteRedundancy = new HashMap<String, Double>();

    // create selected test cases list:
    ActiveTSContainerInfo activeTSContainer =
      CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
    if (activeTSContainer != null) {
      this.selectedTestCases.addAll(sortTestCases(activeTSContainer.getActiveTestCases()));
    }

    // create whole test cases list:
    TestSessionContainer testSessionContainer = activeTSContainer.getTestSessionContainer();
    for (TestSession testSession : testSessionContainer.getTestSessions()) {
      this.wholeTestCases.addAll(testSession.getTestCases());
    }

    // create redundant test cases list:
    activeTSContainer = CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
    if (activeTSContainer != null) {
      if (activeTSContainer.getRedundantTestCases() != null) {
        this.RedundantTestCases.addAll(sortTestCases(activeTSContainer.getRedundantTestCases()));
      }
    }

    CodeCoverPlugin.getDefault().getTSContainerManager().addListener(new TSManagerListener());
  }

  protected static boolean checkTestCases(List<TestCase> testCases) {
    /* Check, if all the test cases share the same test session container */
    for (int i = 0; i < testCases.size() - 1; i++) {
      if (!testCases.get(i).getTestSession().getTestSessionContainer().equals(
        testCases.get(i + 1).getTestSession().getTestSessionContainer())) {
        return false;
      }
    }
    return true;
  }

  /*
   * (non-Javadoc)
   *
   * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  @Override
  public void createPartControl(Composite parent) {
    this.correlationActions.addAll(createCorrelationActions());
    this.TestLevels.addAll(createTestLevelsActions());
    this.MouseStyle.addAll(createMouseStyleActions());
    initializeToolBar();

    this.sashForm = new SashForm(parent, SWT.HORIZONTAL);
    org.eclipse.swt.graphics.Point s = new Point(1000, 500);
    this.graphComposite = new GraphComposite(this.sashForm, SWT.NONE, s, null, ""); //$NON-NLS-1$
    this.graphComposite.setLayout(new GridLayout(2, false));
    this.graphComposite.setLayout(new FillLayout());

    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus() {
  }

  private final void initializeToolBar() {
    IToolBarManager toolBarManager = getViewSite().getActionBars().getToolBarManager();
    toolBarManager.add(createActionChooseMetric());
    toolBarManager.add(createActionChooseMouseStyle());
    toolBarManager.add(createActionDrawGraph());
    toolBarManager.add(createActionCSVExport());
  }

  private final IAction createActionChooseMetric() {
    IAction chooseMetricAction = new Action(LABEL_CHOOSE_COVERAGE_CRITERIA, IAction.AS_DROP_DOWN_MENU) {

      @Override
      public void run() {
        // onCalculateCorrelation();
      }
    };
    chooseMetricAction.setDescription(LABEL_CHOOSE_COVERAGE_CRITERIA);
    chooseMetricAction.setToolTipText(LABEL_CHOOSE_COVERAGE_CRITERIA);
    chooseMetricAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.COVERAGE_CRITERIA.getPath()));
    chooseMetricAction.setMenuCreator(new MetricMenuCreator());

    return chooseMetricAction;
  }

  private final IAction createActionCSVExport() {
    IAction exportCSVAction = new Action(LABEL_CSV_EXPORT_ACTION, IAction.AS_PUSH_BUTTON) {

      @Override
      public void run() {
        onExportCSV();
      }
    };
    exportCSVAction.setDescription(LABEL_CSV_EXPORT_ACTION);
    exportCSVAction.setToolTipText(LABEL_CSV_EXPORT_ACTION);
    exportCSVAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.CSV_EXPORT2.getPath()));

    return exportCSVAction;
  }

  private final IAction createActionDrawGraph() {
    IAction drawGraphAction = new Action(LABEL_DRAW_GRAPH_ACTION, IAction.AS_PUSH_BUTTON) {

      @Override
      public void run() {
        onDrawGraph();
      }
    };
    drawGraphAction.setDescription(LABEL_DRAW_GRAPH_ACTION);
    drawGraphAction.setToolTipText(LABEL_DRAW_GRAPH_ACTION);
    drawGraphAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.DRAW_GRAPH.getPath()));

    return drawGraphAction;
  }

  private final IAction createActionChooseMouseStyle() {
    IAction chooseMouseStyle = new Action(LABEL_CHOOSE_MOUSE_STYLE, IAction.AS_DROP_DOWN_MENU) {

      @Override
      public void run() {
        // onCalculateCorrelation();
      }
    };
    chooseMouseStyle.setDescription(LABEL_CHOOSE_MOUSE_STYLE);
    chooseMouseStyle.setToolTipText(LABEL_CHOOSE_MOUSE_STYLE);
    chooseMouseStyle.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.MOUSE_STYLE.getPath()));
    chooseMouseStyle.setMenuCreator(new MouseStyleMenuCreator());

    return chooseMouseStyle;
  }

  private final void onDrawGraph() {
    // Calculate Redundancy data:
    this.SuiteRedundancy.clear();
    this.PairRedundancy.clear();
    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);

    // Draw Redundancy Graph:
    org.eclipse.swt.graphics.Point size = this.graphComposite.getSize();
    this.graphComposite.dispose();
    this.graphComposite = new GraphComposite(this.sashForm, SWT.NONE, size, createGraph(), this.SelectedMouseStyle);
    if (this.graphComposite.getLayout() != null) {
      this.graphComposite.setLayout(new GridLayout(2, false));
      this.graphComposite.setLayout(new FillLayout());
      this.graphComposite.setSize(size);
    }
  }

  private final void onExportCSV() {
    FileDialog fileDialog = new FileDialog(getSite().getShell(), SWT.SAVE);
    fileDialog.setText(LABEL_CSV_EXPORT_ACTION);
    fileDialog.setFilterExtensions(new String[] {"*.csv", "*.*"}); //$NON-NLS-1$//$NON-NLS-2$

    String location1 = fileDialog.open();
    String location2 = location1;

    try {
      if (location1 != null) {
        File SuiteFile = new File(location1.replaceFirst(".csv", "Suite") + ".csv");
        PrintWriter pw = new PrintWriter(new FileWriter(SuiteFile));
        pw.println("Test Name," + this.ActiveCriterion + " Redundancy, Manual Result");
        for (int i = 0; i < this.selectedTestCases.size(); i++) {
          TestCase currentTestCase = this.selectedTestCases.get(i);
          if (this.RedundantTestCases.contains(currentTestCase)) {
            pw.println(currentTestCase.getName() + "," + this.SuiteRedundancy.get(currentTestCase.getName())
              + ", true");
          } else {
            pw.println(currentTestCase.getName() + "," + this.SuiteRedundancy.get(currentTestCase.getName())
              + ", false");
          }
        }

        pw.close();
        File PairFile = new File(location2.replaceFirst(".csv", "Pair") + ".csv");
        pw = new PrintWriter(new FileWriter(PairFile));
        pw.println("Test Name, With Respect to," + this.ActiveCriterion + " Redundancy");
        List<TestCasePair> PairList = new ArrayList<TestCasePair>(this.PairRedundancy.keySet());
        for (int i = 0; i < PairList.size(); i++) {
          pw.println(PairList.get(i).CurrentTest + "," + PairList.get(i).WithRespectTo + ","
            + this.PairRedundancy.get(PairList.get(i)));
        }
        pw.close();
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  private final IAction createActionChooseTestLevel() {
    IAction chooseTestLevelAction = new Action(LABEL_CHOOSE_TEST_LEVEL, IAction.AS_DROP_DOWN_MENU) {

      @Override
      public void run() {
        // onCalculateCorrelation();
      }
    };
    chooseTestLevelAction.setDescription(LABEL_CHOOSE_TEST_LEVEL);
    chooseTestLevelAction.setToolTipText(LABEL_CHOOSE_TEST_LEVEL);
    chooseTestLevelAction.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.TEST_LEVEL.getPath()));
    chooseTestLevelAction.setMenuCreator(new TestLevelMenuCreator());

    return chooseTestLevelAction;
  }

  private Runnable getTSCChangedRunnable(final ActiveTSContainerInfo tscInfo) {
    return new Runnable() {

      @Override
	public void run() {
        if (tscInfo != null) {
          onTSCChanged(tscInfo.getActiveTestCases(), tscInfo.getRedundantTestCases());
        } else {
          onTSCChanged(new HashSet<TestCase>(), new HashSet<TestCase>());
        }
      }
    };
  }

  private Runnable getActiveTestCasesChangedRunnable(final ActiveTSContainerInfo tscInfo) {
    return new Runnable() {

      @Override
	public void run() {
        if (tscInfo != null) {
          onActiveTestCasesChanged(tscInfo.getActiveTestCases(), tscInfo.getRedundantTestCases());
        } else {
          onActiveTestCasesChanged(new HashSet<TestCase>(), new HashSet<TestCase>());
        }
      }
    };
  }

  private final void refreshActiveTestCaseList(Set<TestCase> selectedtestCases,
    Set<TestCase> redundanttestCases) {
    this.selectedTestCases.clear();
    this.selectedTestCases.addAll(sortTestCases(selectedtestCases));

    // this.RedundantTestCases.clear();
    this.RedundantTestCases.addAll(sortTestCases(redundanttestCases));
  }

  private final void onTSCChanged(Set<TestCase> selectedtestCases, Set<TestCase> redundanttestCases) {
    synchronized (this.lock) {
      this.calculationPending = false;
    }
    refreshActiveTestCaseList(selectedtestCases, redundanttestCases);
    // Clear Redundancy Graph:
    org.eclipse.swt.graphics.Point size = this.graphComposite.getSize();
    this.graphComposite.dispose();
    this.graphComposite = new GraphComposite(this.sashForm, SWT.NONE, size, null, "");
    this.graphComposite.setLayout(new GridLayout(2, false));
    this.graphComposite.setLayout(new FillLayout());
    this.graphComposite.setSize(size);
    this.graphComposite.setRedraw(true);

    // Calculate Redundancy data:
    this.SuiteRedundancy.clear();
    this.PairRedundancy.clear();
    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);
  }

  private final void onActiveTestCasesChanged(Set<TestCase> selectedtestCases,
    Set<TestCase> redundanttestCases) {
    refreshActiveTestCaseList(selectedtestCases, redundanttestCases);
    // Clear Redundancy Graph:
    org.eclipse.swt.graphics.Point size = this.graphComposite.getSize();
    this.graphComposite.dispose();
    this.graphComposite = new GraphComposite(this.sashForm, SWT.NONE, size, null, "");
    this.graphComposite.setLayout(new GridLayout(2, false));
    this.graphComposite.setLayout(new FillLayout());
    this.graphComposite.setSize(size);
    this.graphComposite.setRedraw(true);

    // Calculate Redundancy data:
    this.SuiteRedundancy.clear();
    this.PairRedundancy.clear();
    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);
  }

  private final class GraphComposite
    extends Composite {

    VisualizationComposite<RedundancyGraphNode, RedundancyGraphLink> vv;

    Layout<RedundancyGraphNode, RedundancyGraphLink> layout;

    final DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();

    public GraphComposite(Composite parent, int style, org.eclipse.swt.graphics.Point size,
      edu.uci.ics.jung.graph.Graph<RedundancyGraphNode, RedundancyGraphLink> graph, String MouseStyle) {
      super(parent, style);
      setLayout(new GridLayout());

      if (graph != null) {
        this.layout = new ISOMLayout<RedundancyGraphNode, RedundancyGraphLink>(graph);
        this.layout.setSize(new Dimension(size.x, size.y));

        final GraphZoomScrollPane<RedundancyGraphNode, RedundancyGraphLink> panel =
          new GraphZoomScrollPane<RedundancyGraphNode, RedundancyGraphLink>(this, SWT.NONE, this.layout,
            new Dimension(600, 600));
        GridData gridData = new GridData();
        gridData.grabExcessHorizontalSpace = true;
        gridData.grabExcessVerticalSpace = true;
        gridData.horizontalAlignment = GridData.FILL;
        gridData.verticalAlignment = GridData.FILL;
        panel.setLayoutData(gridData);

        this.vv = panel.vv;
        this.vv.setBackground(Color.white);

        // Setting Labels for each node:
        Transformer<RedundancyGraphNode, String> lableTransformer =
          new Transformer<RedundancyGraphNode, String>() {

            @Override
			public String transform(RedundancyGraphNode node) {
              return node.getLable();
            }
          };
        this.vv.getRenderContext().setVertexLabelTransformer(lableTransformer);

        // Setting Tool Tips for each node:
        Transformer<RedundancyGraphNode, String> toolTipTransformer =
          new Transformer<RedundancyGraphNode, String>() {

            @Override
			public String transform(RedundancyGraphNode node) {
              return node.Redundancy.toString();
            }
          };
        this.vv.setVertexToolTipTransformer(toolTipTransformer);

        // Changing the Shape of each node:
        final Rectangle rectangle = new Rectangle();
        Transformer<RedundancyGraphNode, Shape> vertexTransformer =
          new Transformer<RedundancyGraphNode, Shape>() {

            @Override
			public Shape transform(RedundancyGraphNode node) {
              rectangle.setSize(node.getLable().length() * 8, 16);
              return rectangle;
            }
          };
        this.vv.getRenderContext().setVertexShapeTransformer(vertexTransformer);

        // Changing the Color of each node:
        Transformer<RedundancyGraphNode, Paint> vertexPaint = new Transformer<RedundancyGraphNode, Paint>() {

          @Override
		public Paint transform(RedundancyGraphNode node) {
            if (node.type == "Red") {
              return Color.red;
            } else if (node.type == "Yellow") {
              return Color.yellow;
            } else if (node.type == "Black") {
              return Color.black;
            } else if (node.type == "Green") {
              return Color.green;
            } else {
              return Color.white;
            }
          }
        };
        this.vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);

        // Setting Labels for each edge:
        Transformer<RedundancyGraphLink, String> edgeLableTransformer =
          new Transformer<RedundancyGraphLink, String>() {

            @Override
			public String transform(RedundancyGraphLink edge) {
              return edge.getLable();
            }
          };
        this.vv.getRenderContext().setEdgeLabelTransformer(edgeLableTransformer);
        // vv.getRenderContext().setEdgeShapeTransformer(new
        // EdgeShape.Line<RedundancyGraphNode,RedundancyGraphLink>());

        this.vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR);

        GridData gd = new GridData();
        gd.grabExcessHorizontalSpace = true;
        gd.grabExcessVerticalSpace = true;
        gd.horizontalAlignment = GridData.FILL;
        gd.verticalAlignment = GridData.FILL;
        this.vv.getComposite().setLayoutData(gd);

        this.vv.setGraphMouse(this.graphMouse);
        this.vv.addKeyListener(this.graphMouse.getModeKeyListener());

        final ScalingControl scaler = new CrossoverScalingControl();
        this.vv.scaleToLayout(scaler);

        if (MouseStyle.compareTo("Picking") == 0) {
          this.graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING);
        } else {
          this.graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.TRANSFORMING);
        }
      }
    }

    public void SetMouseStyle(String Style) {
      if (Style.compareTo("Picking") == 0) {
        this.graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING);
      } else {
        this.graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.TRANSFORMING);
      }
    }
    /**
     *
     */

  }

  private final class TSManagerListener
    implements TSContainerManagerListener {

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo,
     *      ChangeType, TestCase)
     */
    @Override
	public void testCaseChanged(final ActiveTSContainerInfo tscInfo, ChangeType changeType, TestCase testCase) {
      Display d = getSite().getShell().getDisplay();
      switch (changeType) {
        case CHANGE:
          synchronized (RedundancyGraphView.this.lock) {
            if (RedundancyGraphView.this.calculationPending) {
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
      synchronized (RedundancyGraphView.this.lock) {
        if (RedundancyGraphView.this.calculationPending) {
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
	public void testSessionChanged(final ActiveTSContainerInfo tscInfo, ChangeType changeType,
      TestSession testSession) {
      Display disp = getSite().getShell().getDisplay();

      synchronized (RedundancyGraphView.this.lock) {
        if (RedundancyGraphView.this.calculationPending) {
          return;
        }
        RedundancyGraphView.this.calculationPending = true;

        disp.asyncExec(getTSCChangedRunnable(tscInfo));
      }
    }

    /*
     * (non-Javadoc)
     *
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
     */
    @Override
	public void testSessionContainerActivated(final ActiveTSContainerInfo tscInfo) {
      Display disp = getSite().getShell().getDisplay();

      synchronized (RedundancyGraphView.this.lock) {
        if (RedundancyGraphView.this.calculationPending) {
          return;
        }
        RedundancyGraphView.this.calculationPending = true;

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
	public void testSessionContainerChanged(ChangeType changeType, final ActiveTSContainerInfo tscInfo) {
      Display disp = getSite().getShell().getDisplay();

      synchronized (RedundancyGraphView.this.lock) {
        if (RedundancyGraphView.this.calculationPending) {
          return;
        }
        RedundancyGraphView.this.calculationPending = true;

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
     * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#synchronizedStateChanged(TSContainerInfo,
     *      boolean)
     */
    @Override
	public void synchronizedStateChanged(TSContainerInfo tscInfo, boolean isSynchronized) {
      // We don't react on this.
    }
  }

  private final void onChooseMetric(String Criterion) {
    // Clear Redundancy Graph:
    org.eclipse.swt.graphics.Point size = this.graphComposite.getSize();
    this.graphComposite.dispose();
    this.graphComposite = new GraphComposite(this.sashForm, SWT.NONE, size, null, "");
    this.graphComposite.setLayout(new GridLayout(2, false));
    this.graphComposite.setLayout(new FillLayout());
    this.graphComposite.setSize(size);
    this.graphComposite.setRedraw(true);

    // Calculate Redundancy data:
    this.SuiteRedundancy.clear();
    this.PairRedundancy.clear();
    this.ActiveCriterion = Criterion;
    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);
  }

  private final void onChooseTestLevel(String level) {
    // Calculate Redundancy data:
    this.SuiteRedundancy.clear();
    this.PairRedundancy.clear();
    calculateSuiteRedundancy(this.ActiveCriterion);
    calculatePairRedundancy(this.ActiveCriterion);
  }

  private final void onChooseMouseStyle(String selectedStyle) {
    this.SelectedMouseStyle = selectedStyle;
    this.graphComposite.SetMouseStyle(selectedStyle);
  }

  private final void calculateSuiteRedundancy(String Criterion) {
    Set<CoverableItem> coverableItemSet = createCoverableItemSet(Criterion);
    Set<CoverableItem> mainSet = new HashSet<CoverableItem>(coverableItemSet);
    Set<CoverableItem> otherTestSet = new HashSet<CoverableItem>();
    if (Criterion.compareTo("Condition") != 0) {
      for (int i = 0; i < this.wholeTestCases.size(); i++) {
        TestCase currentTestCase = this.wholeTestCases.get(i);
        Set<CoverableItem> currentCoveredSet =
          new HashSet<CoverableItem>(currentTestCase.getCoverageData().keySet());
        currentCoveredSet.retainAll(mainSet);
        int ts = currentCoveredSet.size();
        otherTestSet.clear();
        for (int a = 0; a < this.wholeTestCases.size(); a++) {
          if (a != i) {
            TestCase secondTestCase = this.wholeTestCases.get(a);
            if (!this.RedundantTestCases.contains(secondTestCase)) {
              Set<CoverableItem> secondSet =
                new HashSet<CoverableItem>(secondTestCase.getCoverageData().keySet());
              secondSet.retainAll(mainSet);
              otherTestSet.addAll(secondSet);
            }
          }
        }

        currentCoveredSet.retainAll(otherTestSet);
        double rts = currentCoveredSet.size();
        Double SuiteR = 0.0D;
        if (ts != 0.0D) {
          SuiteR = rts / ts;
        } else {
          SuiteR = (0.0D / 0.0D);
        }

        if (!SuiteR.isNaN()) {
          BigDecimal bd = new BigDecimal(SuiteR);
          bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
          SuiteR = bd.doubleValue();
        }
        if (this.RedundantTestCases.contains(currentTestCase)) {
          this.SuiteRedundancy.put(currentTestCase.getName(), 1.0);
        } else {
          this.SuiteRedundancy.put(currentTestCase.getName(), SuiteR);
        }
      }
    } else {
      for (int i = 0; i < this.wholeTestCases.size(); i++) {
        TestCase currentTestCase = this.wholeTestCases.get(i);
        Set<CoverableItem> currentCoveredSet = new HashSet<CoverableItem>(currentTestCase.getAssignmentsMap().keySet());
        currentCoveredSet.retainAll(mainSet);
        int amountFirstTestCase = 0;
        for (CoverableItem item : currentCoveredSet) {
          BooleanAssignmentMap map = currentTestCase.getAssignmentsMap().get(item);
          amountFirstTestCase += map.getEvaluatedAssignments().size();
        }

        int ts = amountFirstTestCase;
        int amountIntersection = 0;
        for (CoverableItem item : currentCoveredSet) {
          BooleanAssignmentMap map1 = currentTestCase.getAssignmentsMap().get(item);
          Set<BooleanAssignment> sharedAssignments =
            new HashSet<BooleanAssignment>(map1.getEvaluatedAssignments());
          Set<BooleanAssignment> otherAssignments = new HashSet<BooleanAssignment>();
          for (int a = 0; a < this.wholeTestCases.size(); a++) {
            if (a != i) {
              TestCase secondTestCase = this.wholeTestCases.get(a);
              if (!this.RedundantTestCases.contains(secondTestCase)) {
                Set<CoverableItem> secondSet =
                  new HashSet<CoverableItem>(secondTestCase.getAssignmentsMap().keySet());
                secondSet.retainAll(mainSet);
                if (secondSet.contains(item)) {
                  BooleanAssignmentMap map2 = secondTestCase.getAssignmentsMap().get(item);
                  otherAssignments.addAll(map2.getEvaluatedAssignments());
                }
              }
            }
          }

          sharedAssignments.retainAll(otherAssignments);
          amountIntersection += sharedAssignments.size();
        }

        double rts = amountIntersection;
        Double SuiteR = 0.0D;
        if (ts != 0.0D) {
          SuiteR = rts / ts;
        } else {
          SuiteR = (0.0D / 0.0D);
        }

        if (!SuiteR.isNaN()) {
          BigDecimal bd = new BigDecimal(SuiteR);
          bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
          SuiteR = bd.doubleValue();
        }
        this.SuiteRedundancy.put(currentTestCase.getName(), SuiteR);
      }
    }
  }

  private final void calculatePairRedundancy(String Criterion) {
    Set<CoverableItem> coverableItemSet = createCoverableItemSet(Criterion);
    Set<CoverableItem> mainSet = new HashSet<CoverableItem>(coverableItemSet);
    if (Criterion.compareTo("Condition") != 0) {
      for (int i = 0; i < this.wholeTestCases.size(); i++) {
        TestCase currentTestCase = this.wholeTestCases.get(i);
        Set<CoverableItem> currentCoveredSet = new HashSet<CoverableItem>(currentTestCase.getCoverageData().keySet());
        currentCoveredSet.retainAll(mainSet);
        double ts = currentCoveredSet.size();

        double rts;
        Double PairR = 0.0D;
        for (int a = 0; a < this.wholeTestCases.size(); a++) {
          if (a != i) {
            TestCase secondTestCase = this.wholeTestCases.get(a);
            Set<CoverableItem> secondSet = new HashSet<CoverableItem>(secondTestCase.getCoverageData().keySet());
            secondSet.retainAll(mainSet);
            secondSet.retainAll(currentCoveredSet);
            rts = secondSet.size();
            PairR = 0.0D;
            if (ts != 0.0D) {
              PairR = rts / ts;
            } else {
              PairR = (0.0D / 0.0D);
            }
            TestCasePair tcp = new TestCasePair();
            tcp.CurrentTest = currentTestCase.getName();
            tcp.WithRespectTo = secondTestCase.getName();
            if (this.selectedTestCases.contains(currentTestCase)) {
              tcp.selected = true;
            } else {
              tcp.selected = false;
            }
            if (!PairR.isNaN()) {
              BigDecimal bd = new BigDecimal(PairR);
              bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
              PairR = bd.doubleValue();
            }
            this.PairRedundancy.put(tcp, PairR);
          }
        }
      }
    } else {
      Map<TestCase, Integer> totalCoverableItemCount = new HashMap<TestCase, Integer>();
      for (int i = 0; i < this.wholeTestCases.size(); i++) {
        TestCase currentTestCase = this.wholeTestCases.get(i);
        Set<CoverableItem> currentCoveredSet = new HashSet<CoverableItem>(currentTestCase.getAssignmentsMap().keySet());
        currentCoveredSet.retainAll(mainSet);
        int amountFirstTestCase = 0;
        for (CoverableItem item : currentCoveredSet) {
          BooleanAssignmentMap map = currentTestCase.getAssignmentsMap().get(item);
          amountFirstTestCase += map.getEvaluatedAssignments().size();
        }

        double ts = amountFirstTestCase;
        totalCoverableItemCount.put(currentTestCase, Integer.valueOf(amountFirstTestCase));
        for (int a = 0; a < this.wholeTestCases.size(); a++) {
          if (a != i) {
            TestCase secondTestCase = this.wholeTestCases.get(a);
            Set<CoverableItem> secondSet = new HashSet<CoverableItem>(secondTestCase.getAssignmentsMap().keySet());
            secondSet.retainAll(mainSet);
            secondSet.retainAll(currentCoveredSet);
            int amountIntersection = 0;
            for (CoverableItem item : secondSet) {
              BooleanAssignmentMap map1 = currentTestCase.getAssignmentsMap().get(item);
              BooleanAssignmentMap map2 = secondTestCase.getAssignmentsMap().get(item);
              Set<BooleanAssignment> sharedAssignments = new HashSet<BooleanAssignment>(map1.getEvaluatedAssignments());
              sharedAssignments.retainAll(map2.getEvaluatedAssignments());
              amountIntersection += sharedAssignments.size();
            }

            double rts = amountIntersection;
            Double PairR = 0.0D;
            if (ts != 0.0D) {
              PairR = rts / ts;
            } else {
              PairR = (0.0D / 0.0D);
            }
            TestCasePair tcp = new TestCasePair();
            tcp.CurrentTest = currentTestCase.getName();
            tcp.WithRespectTo = secondTestCase.getName();
            if (!PairR.isNaN()) {
              BigDecimal bd = new BigDecimal(PairR);
              bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
              PairR = bd.doubleValue();
            }
            this.PairRedundancy.put(tcp, PairR);
          }
        }
      }
    }
  }

  private final Set<CoverableItem> createCoverableItemSet(String Criterion) {
    if (this.wholeTestCases.size() == 0) {
      throw new IllegalArgumentException("testCases.size() == 0");
    }
    if (!checkTestCases(this.wholeTestCases)) {
      throw new IllegalArgumentException("Not all test cases have the same test session container");
    }
    TestSessionContainer tsc = this.wholeTestCases.get(0).getTestSession().getTestSessionContainer();
    final Set<CoverableItem> coverableItemSet = new HashSet<CoverableItem>();
    if (Criterion.compareTo("Statement") == 0) {
      tsc.getCode().accept(null, null, new Statement.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(BasicStatement statement) {
          add(statement.getCoverableItem());
        }

        @Override
        public void visit(ConditionalStatement statement) {
          add(statement.getCoverableItem());
        }

        @Override
        public void visit(LoopingStatement statement) {
          add(statement.getCoverableItem());
        }
      }, null, null, null, null, null, null);
    } else if (Criterion.compareTo("Branch") == 0) {
      tsc.getCode().accept(null, null, new Statement.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(Branch branch) {
          add(branch.getCoverableItem());
        }
      }, null, null, null, null, null, null);
    } else if (Criterion.compareTo("Loop") == 0) {
      tsc.getCode().accept(null, null, new Statement.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(LoopingStatement statement) {
          if (!statement.isOptionalBodyExecution()) {
            add(statement.getNeverExecutedItem());
          }
          add(statement.getMultipleExecutedItem());
          add(statement.getOnceExecutedItem());
        }
      }, null, null, null, null, null, null);
    } else if (Criterion.compareTo("Condition") == 0) {
      tsc.getCode().accept(null, null, null, null, new RootTerm.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(RootTerm term) {
          add(term.getCoverableItem());
        }
      }, null, null, null, null);
    } else if (Criterion.compareTo("All") == 0) {
      tsc.getCode().accept(null, null, null, null, new RootTerm.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(RootTerm term) {
          add(term.getCoverableItem());
        }
      }, null, null, null, null);
      tsc.getCode().accept(null, null, new Statement.DefaultVisitor() {

        private void add(CoverableItem item) {
          if (item != null) {
            coverableItemSet.add(item);
          }
        }

        @Override
        public void visit(LoopingStatement statement) {
          if (!statement.isOptionalBodyExecution()) {
            add(statement.getNeverExecutedItem());
          }
          add(statement.getMultipleExecutedItem());
          add(statement.getOnceExecutedItem());
        }

        @Override
        public void visit(Branch branch) {
          add(branch.getCoverableItem());
        }

        @Override
        public void visit(BasicStatement statement) {
          add(statement.getCoverableItem());
        }

        @Override
        public void visit(ConditionalStatement statement) {
          add(statement.getCoverableItem());
        }
      }, null, null, null, null, null, null);
    }
    return coverableItemSet;
  }

  // private final void CreateGraph(String Criterion) {
  //
  // if (selectedTestCases.isEmpty()) {
  // redundancyGraph.dispose();
  // }
  //
  // Vector<GraphNode> TestItems = new Vector<GraphNode>();
  // Vector<String> TestItemsId = new Vector<String>();
  // String TestNodeName="";
  // GraphConnection connection;
  //
  // //Drawing Suite Redundancy:
  // for(int i = 0; i < selectedTestCases.size(); i++)
  // {
  // TestCase tc = (TestCase)selectedTestCases.get(i);
  // TestNodeName = tc.getName();
  //
  // TestItemsId.add(TestNodeName);
  // TestItems.add(new GraphNode(redundancyGraph, SWT.ALPHA, TestNodeName));
  // connection = new GraphConnection(redundancyGraph, SWT.NONE, TestItems.lastElement(),
  // TestItems.lastElement());
  // connection.setText(SuiteRedundancy.get(TestNodeName).toString());
  // if(SuiteRedundancy.get(TestNodeName) == 1 && RedundantTestCases.contains(tc)){
  // org.eclipse.swt.graphics.Color c = sashForm.getShell().getDisplay().getSystemColor(SWT.COLOR_RED);
  // TestItems.lastElement().setBackgroundColor(c);
  // }
  // else if(SuiteRedundancy.get(TestNodeName) == 1){
  // org.eclipse.swt.graphics.Color c = sashForm.getShell().getDisplay().getSystemColor(SWT.COLOR_YELLOW);
  // TestItems.lastElement().setBackgroundColor(c);
  // }
  // else if(SuiteRedundancy.get(TestNodeName).isNaN()) {
  // org.eclipse.swt.graphics.Color c = sashForm.getShell().getDisplay().getSystemColor(SWT.COLOR_BLACK);
  // TestItems.lastElement().setBackgroundColor(c);
  // }
  // else{
  // org.eclipse.swt.graphics.Color c = sashForm.getShell().getDisplay().getSystemColor(SWT.COLOR_GREEN);
  // TestItems.lastElement().setBackgroundColor(c);
  // }
  // }
  //
  // //Drawing Pair Redundancy:
  // List<TestCasePair> PairList = new ArrayList(PairRedundancy.keySet());
  // for(int i=0;i<PairList.size();i++){
  // Double d = PairRedundancy.get(PairList.get(i));
  // if(PairList.get(i).selected)
  // if(d != 0 && !d.isNaN()){
  // if(!TestItemsId.contains(PairList.get(i).WithRespectTo)){
  // TestItemsId.add(PairList.get(i).WithRespectTo);
  // TestItems.add(new GraphNode(redundancyGraph, SWT.ALPHA, PairList.get(i).WithRespectTo));
  // org.eclipse.swt.graphics.Color c = sashForm.getShell().getDisplay().getSystemColor(SWT.COLOR_WHITE);
  // TestItems.lastElement().setBackgroundColor(c);
  // }
  // connection = new GraphConnection(redundancyGraph, SWT.Arm,
  // TestItems.elementAt(TestItemsId.indexOf(PairList.get(i).CurrentTest)),
  // TestItems.elementAt(TestItemsId.indexOf(PairList.get(i).WithRespectTo)));
  // connection.setText(d.toString());
  // }
  // }
  // }
  class RedundancyGraphLink {

    int id;

    Double PairRedundancy;

    public RedundancyGraphLink(int id, Double PR) {
      this.PairRedundancy = PR;
      this.id = id;
    }

    public String getLable() {
      return this.PairRedundancy.toString();
    }
  }

  private edu.uci.ics.jung.graph.Graph<RedundancyGraphNode, RedundancyGraphLink> createGraph() {

    if (this.selectedTestCases.size() != 0) {
      edu.uci.ics.jung.graph.Graph<RedundancyGraphNode, RedundancyGraphLink> graph =
        new SparseMultigraph<RedundancyGraphNode, RedundancyGraphLink>();
      Vector<RedundancyGraphNode> TestItems = new Vector<RedundancyGraphNode>();
      Vector<String> TestItemsId = new Vector<String>();
      String TestNodeName = "";
      // GraphConnection connection;

      int NumofNodes = 0;
      try {

        // Drawing Suite Redundancy:
        for (int i = 0; i < this.selectedTestCases.size(); i++) {
          TestCase tc = this.selectedTestCases.get(i);
          TestNodeName = tc.getName();

          RedundancyGraphNode node = new RedundancyGraphNode(TestNodeName, this.SuiteRedundancy.get(TestNodeName));
          TestItemsId.add(TestNodeName);
          TestItems.add(node);
          graph.addVertex(node);
          NumofNodes++;

          // I don't know how to set color for each node (change it later):
          if (this.SuiteRedundancy.get(TestNodeName) == 1 && this.RedundantTestCases.contains(tc)) {
            node.type = "Red";
          } else if (this.SuiteRedundancy.get(TestNodeName) == 1) {
            node.type = "Yellow";
          } else if (this.SuiteRedundancy.get(TestNodeName).isNaN()) {
            node.type = "Black";
          } else {
            node.type = "Green";
          }
        }

        // Drawing Pair Redundancy:
        List<TestCasePair> PairList = new ArrayList<TestCasePair>(this.PairRedundancy.keySet());
        for (int i = 0; i < PairList.size(); i++) {
          Double d = this.PairRedundancy.get(PairList.get(i));
          if (PairList.get(i).selected) {
            if (d != 0 && !d.isNaN()) {
              RedundancyGraphNode WithRespectToNode = null;
              if (!TestItemsId.contains(PairList.get(i).WithRespectTo)) {
                WithRespectToNode = new RedundancyGraphNode(PairList.get(i).WithRespectTo, 1000.0);
                TestItemsId.add(PairList.get(i).WithRespectTo);
                TestItems.add(WithRespectToNode);
                graph.addVertex(WithRespectToNode);
                NumofNodes++;
                WithRespectToNode.type = "White";
              } else {
                for (int k = 0; k < NumofNodes; k++) {
                  if (TestItems.get(k).TestName.compareTo(PairList.get(i).WithRespectTo) == 0) {
                    WithRespectToNode = TestItems.get(k);
                  }
                }
              }
              RedundancyGraphNode CurrentTestNode = null;
              for (int k = 0; k < NumofNodes; k++) {
                if (TestItems.get(k).TestName.compareTo(PairList.get(i).CurrentTest) == 0) {
                  CurrentTestNode = TestItems.get(k);
                }
              }

              RedundancyGraphLink link = new RedundancyGraphLink(i, d);
              graph.addEdge(link, CurrentTestNode, WithRespectToNode, EdgeType.DIRECTED);
            }
          }

        }
      } catch (Exception ex) {
      }
      return graph;
    }
    return null;
  }

  private final List<IAction> createCorrelationActions() {
    IWorkbench workbench = PlatformUI.getWorkbench();
    ISharedImages platformImages = workbench.getSharedImages();

    final List<IAction> correlationActions = new Vector<IAction>();

    // Create Action For Statement Criterion:
    String name = "Statement Coverage";
    IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMetric("Statement");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(true);
    correlationActions.add(action);

    // Create Action For Branch Criterion:
    name = "Branch Coverage";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMetric("Branch");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    correlationActions.add(action);

    // Create Action For Condition Criterion:
    name = "Condition Coverage";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMetric("Condition");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    correlationActions.add(action);

    // Create Action For Loop Criterion:
    name = "Loop Coverage";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMetric("Loop");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    correlationActions.add(action);

    // Create Action For All Criteria:
    name = "All Metrics";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMetric("All");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    correlationActions.add(action);

    return correlationActions;
  }

  private final List<IAction> createTestLevelsActions() {
    IWorkbench workbench = PlatformUI.getWorkbench();
    ISharedImages platformImages = workbench.getSharedImages();

    final List<IAction> TestLevelActions = new Vector<IAction>();

    // Create Action For Package Level:
    String name = "Test Package";
    IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseTestLevel("Package");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    TestLevelActions.add(action);

    // Create Action For Class Level:
    name = "Test Class";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseTestLevel("Class");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    TestLevelActions.add(action);

    // Create Action For Method Level:
    name = "Test Method";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseTestLevel("Method");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(true);
    TestLevelActions.add(action);

    return TestLevelActions;
  }

  private final List<IAction> createMouseStyleActions() {
    IWorkbench workbench = PlatformUI.getWorkbench();
    ISharedImages platformImages = workbench.getSharedImages();

    final List<IAction> MouseStyleActions = new Vector<IAction>();

    // Create Action For Picking:
    String name = "Picking";
    IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMouseStyle("Picking");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(true);
    MouseStyleActions.add(action);

    // Create Action For Transforming:
    name = "Moving";
    action = new Action(name, IAction.AS_RADIO_BUTTON) {

      @Override
      public void run() {
        onChooseMouseStyle("Moving");
      }
    };
    action.setImageDescriptor(platformImages.getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

    action.setChecked(false);
    MouseStyleActions.add(action);
    return MouseStyleActions;
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
   * @see org.eclipse.ui.part.ViewPart#init(org.eclipse.ui.IViewSite, org.eclipse.ui.IMemento)
   */
  @Override
  public void init(IViewSite site, IMemento memento) throws PartInitException {
    super.init(site, memento);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.eclipse.ui.part.ViewPart#saveState(org.eclipse.ui.IMemento)
   */
  @Override
  public void saveState(IMemento memento) {
  }

  private final class TestCaseComparator
    implements Comparator<TestCase> {

    /**
     * Compares two {@link TestCase}s
     *
     * @param testCase1 the first {@link TestCase}
     * @param testCase2 the second {@link TestCase}
     * @return the comparison result of the names of the {@link TestCase}'s {@link TestSession}s, or, if
     *         those names are equal, the comparison result of the names of the {@link TestCase}s
     */
    @Override
	public int compare(TestCase testCase1, TestCase testCase2) {
      int result;
      result = testCase1.getTestSession().getName().compareTo(testCase2.getTestSession().getName());

      if (result == 0) {
        result = testCase1.getName().compareTo(testCase2.getName());
      }

      return result;
    }
  }

  public final class TestCasePair {

    String CurrentTest;

    String WithRespectTo;

    boolean selected;
  }

  private final class MetricMenuCreator
    implements IMenuCreator {

    @Override
	public void dispose() {
      // Nothing to do here.
    }

    @Override
	public Menu getMenu(Control parent) {
      Menu dropDownMenu = new Menu(parent);
      for (IAction action : RedundancyGraphView.this.correlationActions) {

        ActionContributionItem contributionItem = new ActionContributionItem(action);

        contributionItem.fill(dropDownMenu, -1);

      }
      return dropDownMenu;
    }

    @Override
	public Menu getMenu(Menu parent) {
      return null;
    }
  }

  private final class TestLevelMenuCreator
    implements IMenuCreator {

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
      for (IAction action : RedundancyGraphView.this.TestLevels) {

        ActionContributionItem contributionItem = new ActionContributionItem(action);

        contributionItem.fill(dropDownMenu, -1);

      }
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

  private final class MouseStyleMenuCreator
    implements IMenuCreator {

    @Override
	public void dispose() {
      // Nothing to do here.
    }

    @Override
	public Menu getMenu(Control parent) {
      Menu dropDownMenu = new Menu(parent);
      for (IAction action : RedundancyGraphView.this.MouseStyle) {

        ActionContributionItem contributionItem = new ActionContributionItem(action);

        contributionItem.fill(dropDownMenu, -1);

      }
      return dropDownMenu;
    }

    @Override
	public Menu getMenu(Menu parent) {
      return null;
    }
  }
}