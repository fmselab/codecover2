/******************************************************************************
 * Copyright (c) 2009 Negar Koochakzadeh, Vahid Garousi			      *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.views;

import java.lang.reflect.InvocationTargetException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.views.controls.DeleteTSCsSelectDialog;
import org.codecover.eclipse.views.controls.DeleteTestElementsConfirmDialog;
import org.codecover.eclipse.views.controls.DeleteTestElementsRunnable;
import org.codecover.eclipse.views.controls.DeleteTestElementsSelectDialog;
import org.codecover.eclipse.views.controls.MergeWizard;
import org.codecover.eclipse.views.controls.RedundancyViewerFactory;
import org.codecover.eclipse.views.controls.SaveActiveTSContainerAction;
import org.codecover.eclipse.views.controls.TestElementPropertiesDialog;
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
import org.codecover.model.utils.LogLevel;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CheckStateChangedEvent;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.ICheckStateListener;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;

/**
 * This {@link ManualRedundancyView} is a view in eclipse. It provides the user with the opportunity to
 * identify a test case as a redundant one. This manual redundancy detection effects on redundancy metrics and
 * redundancy graph. Project supervisor: Vahid Garousi (http://www.ucalgary.ca/~vgarousi/) Software Quality
 * Engineering Research Group (SoftQual) Department of Electrical and Computer Engineering Schulich School of
 * Engineering University of Calgary, Alberta, Canada http://www.softqual.ucalgary.ca
 *
 * @author Negar Koochakzadeh
 * @version 1.0
 */
public class ManualRedundancyView
  extends ViewPart {

  private static final String DIALOG_ERROR_INCONSISTENCY_TITLE =
    Messages.getString("TestSessionsView.DIALOG_ERROR_INCONSISTENCY_TITLE"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_INCONSISTENCY_MSG =
    Messages.getString("TestSessionsView.DIALOG_ERROR_INCONSISTENCY_MSG"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_LOADING_TSC_TITLE =
    Messages.getString("TestSessionsView.DIALOG_ERROR_LOADING_TSC_TITLE"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_LOADING_TSC_MSG =
    Messages.getString("TestSessionsView.DIALOG_ERROR_LOADING_TSC_MSG"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_TITLE =
    Messages.getString("TestSessionsView.DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_TITLE"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_MSG =
    Messages.getString("TestSessionsView.DIALOG_ERROR_ACTIVATING_TSC_OUT_OF_MEM_MSG"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_UNKNOWN_TSC_SELECTED_TITLE =
    Messages.getString("TestSessionsView.DIALOG_ERROR_UNKNOWN_TSC_SELECTED_TITLE"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_UNKNOWN_TSC_SELECTED_MSG =
    Messages.getString("TestSessionsView.DIALOG_ERROR_UNKNOWN_TSC_SELECTED_MSG"); //$NON-NLS-1$

  private static final String TSC_DELETE_ACTION_TEXT =
    Messages.getString("TestSessionsView.TSC_DELETE_ACTION_TEXT"); //$NON-NLS-1$

  private static final String TSC_DELETE_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.TSC_DELETE_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String TSC_DELETE_MULTIPLE_ACTION_TEXT =
    Messages.getString("TestSessionsView.TSC_DELETE_MULTIPLE_ACTION_TEXT"); //$NON-NLS-1$

  private static final String TSC_DELETE_MULTIPLE_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.TSC_DELETE_MULTIPLE_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String TSC_DELETE_DIALOG_TITLE =
    Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_TITLE"); //$NON-NLS-1$

  private static final String TSC_DELETE_DIALOG_MSG =
    Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_MSG"); //$NON-NLS-1$

  private static final String TSC_DELETE_DIALOG_ERROR_TITLE =
    Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_ERROR_TITLE"); //$NON-NLS-1$

  private static final String TSC_DELETE_DIALOG_ERROR_MSG =
    Messages.getString("TestSessionsView.TSC_DELETE_DIALOG_ERROR_MSG"); //$NON-NLS-1$

  private static final String ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU =
    Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU"); //$NON-NLS-1$

  private static final String ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR =
    Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR"); //$NON-NLS-1$

  private static final String ELEMENTS_DELETE_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.ELEMENTS_DELETE_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_ELEMENT_DELETE_TITLE =
    Messages.getString("TestSessionsView.DIALOG_ERROR_ELEMENT_DELETE_TITLE"); //$NON-NLS-1$

  private static final String DIALOG_ERROR_ELEMENT_DELETE_MSG =
    Messages.getString("TestSessionsView.DIALOG_ERROR_ELEMENT_DELETE_MSG"); //$NON-NLS-1$

  private static final String MERGE_ACTION_TEXT = Messages.getString("TestSessionsView.MERGE_ACTION_TEXT"); //$NON-NLS-1$

  private static final String MERGE_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.MERGE_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String SELECT_ALL_ACTION_TEXT =
    Messages.getString("TestSessionsView.SELECT_ALL_ACTION_TEXT"); //$NON-NLS-1$

  private static final String SELECT_ALL_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.SELECT_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String ACTIVATE_ALL_ACTION_TEXT =
    Messages.getString("TestSessionsView.ACTIVATE_ALL_ACTION_TEXT"); //$NON-NLS-1$

  private static final String ACTIVATE_ALL_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.ACTIVATE_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String DEACTIVATE_ALL_ACTION_TEXT =
    Messages.getString("TestSessionsView.DEACTIVATE_ALL_ACTION_TEXT"); //$NON-NLS-1$

  private static final String DEACTIVATE_ALL_ACTION_TOOLTIP =
    Messages.getString("TestSessionsView.DEACTIVATE_ALL_ACTION_TOOLTIP"); //$NON-NLS-1$

  private static final String PROPERTIES_ACTION_TEXT =
    Messages.getString("TestSessionsView.PROPERTIES_ACTION_TEXT"); //$NON-NLS-1$

  /**
   * The <code>TSContainerInfo</code>-representations of the test session containers which are listed in
   * the combo box (and known to this view).
   */
  private List<TSContainerInfo> tscInfos;

  /**
   * The <code>TSContainerInfo</code>-representation of the test session container which is currently
   * visualized in the viewer.
   */
  private TSContainerInfo tscInfo;

  /**
   * The (active) <code>TestCase</code>s which are currently visualized in the viewer as being active (by
   * checked checkboxes).
   */
  private Set<TestCase> RedundanttestCases;

  private final List<TestCase> wholeTestCases = new Vector<TestCase>();

  private CheckboxTreeViewer viewer;

  // private Combo combo;

  /**
   * Saves the active test session container.
   */
  private Action saveTSC;

  /**
   * Deletes one or multiple test session containers.
   */
  private Action deleteTSC;

  /**
   * Deletes test elements and is added to the toolbar.
   */
  private Action deleteToolBar;

  /**
   * Deletes test elements and is added to the context menu of the tree viewer.
   */
  private Action deleteContextMenu;

  /**
   * Merges test elements.
   */
  private Action merge;

  /**
   * Context menu item of the tree viewer which selects all test elements.
   */
  private Action selectAll;

  /**
   * Context menu item of the tree viewer which activates all test cases.
   */
  private Action activateAll;

  /**
   * Context menu item of the tree viewer which deactivates all test cases.
   */
  private Action deactivateAll;

  /**
   * Context menu item of the tree viewer which allows the user to edit the name and comment of a test
   * element.
   */
  private Action propertiesAction;

  /**
   * Debug action which is added to the pulldown if the plugin's logger is in debug mode.
   *
   * @see CodeCoverPlugin#getLogLevel()
   */
  private Action showActiveTestCases;

  /**
   * Saves the expanded elements of the viewer, which are only <code>TestSession</code>s. Thus only the
   * names of the expanded <code>TestSession</code>s of a <code>TSContainerInfo</code> are saved.
   */
  private final Map<TSContainerInfo, Set<String>> expandedElements;

  private final Logger logger;

  /**
   * The constructor.
   */
  public ManualRedundancyView() {
    this.tscInfos = new LinkedList<TSContainerInfo>();
    this.RedundanttestCases = new HashSet<TestCase>();
    this.expandedElements = new HashMap<TSContainerInfo, Set<String>>();
    this.logger = CodeCoverPlugin.getDefault().getLogger();

    // create whole test cases list:
    ActiveTSContainerInfo activeTSContainer =
      CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
    TestSessionContainer testSessionContainer = activeTSContainer.getTestSessionContainer();
    for (TestSession testSession : testSessionContainer.getTestSessions()) {
      this.wholeTestCases.addAll(testSession.getTestCases());
    }

    CalculateSuiteRedundancy("Statement");
    CalculateSuiteRedundancy("Branch");
    CalculateSuiteRedundancy("Condition");
    CalculateSuiteRedundancy("Loop");
    CalculateSuiteRedundancy("All");
  }

  /*
   * methods to set and get the data which is visualized by this view
   */

  /**
   * Sets the input of the viewer to the given data.
   */
  private void setViewerInput(ActiveTSContainerInfo tscInfo) {
    // save expanded elements of old test session container
    saveExpandedElements(this.getVisTSCInfo());

    if (tscInfo != null) {
      this.tscInfo = tscInfo.getTSContainerInfo();
      this.saveTSC.setEnabled(!this.tscInfo.isSynchronized());
      // the given set is immutable thus we need to copy it
      // this.RedundanttestCases =new HashSet<TestCase>(tscInfo.getRedundantTestCases());
      this.viewer.setInput(tscInfo.getTestSessionContainer());
    } else {
      this.saveTSC.setEnabled(false);
      this.tscInfo = null;

      this.viewer.setInput(null);
    }
    this.RedundanttestCases = new HashSet<TestCase>();

    // check the checkboxes of the active test cases
    this.checkRedundantTestCases();
    // set expanded elements of newly selected session container
    restoreExpandedElements(tscInfo);

    this.deleteTSC.setEnabled(tscInfo != null);
    this.merge.setEnabled(tscInfo != null);
    this.deleteContextMenu.setEnabled(tscInfo != null);
    this.deleteToolBar.setEnabled(tscInfo != null);
  }

  /**
   * Returns the <code>TSContainerInfo</code>-representations of the test session containers which are
   * currently listed in the combo box.
   *
   * @return the <code>TSContainerInfo</code>-representations of the test session containers which are
   *         currently listed in the combo box.
   */
  private List<TSContainerInfo> getVisTSCInfos() {
    return this.tscInfos;
  }

  /**
   * Returns the <code>TestSessionContainer</code> which is currently visualized in the viewer.
   *
   * @return the <code>TestSessionContainer</code> which is currently visualized in the viewer.
   */
  private TestSessionContainer getVisTSC() {
    return (TestSessionContainer) this.viewer.getInput();
  }

  /**
   * Returns the <code>TSContainerInfo</code>-representation of the test session container which is
   * currently visualized in the viewer.
   *
   * @return the <code>TSContainerInfo</code>-representation of the test session container which is
   *         currently visualized in the viewer.
   */
  private TSContainerInfo getVisTSCInfo() {
    return this.tscInfo;
  }

  /**
   * Returns the <code>TestCase</code>s which are currently visualized in the viewer as being active (by
   * checked checkboxes).
   *
   * @return the <code>TestCase</code>s which are currently visualized in the viewer as being active (by
   *         checked checkboxes).
   */
  private Set<TestCase> getVisTestCases() {
    return this.RedundanttestCases;
  }

  /*
   * methods to create and initialize the controls of the view
   */

  /**
   * This is a callback that will allow us to create the controls of the view and initialize it.
   */
  @Override
  public void createPartControl(Composite parent) {
    // create viewer
    this.viewer = RedundancyViewerFactory.newContainerCheckedTreeViewer(parent, false, new Runnable() {

      @Override
	public void run() {
        // empty
      }
    });

    // create actions and add them to context menu and toolbars
    this.makeActions();
    this.fillActionMenus();
    this.hookDoubleClickAction();

    // layout
    parent.setLayout(new FormLayout());
    FormData formData = new FormData();

    formData = new FormData();
    formData.left = new FormAttachment(0, 5);
    formData.top = new FormAttachment(0, SWT.CENTER);
    // label.setLayoutData(formData);

    formData = new FormData();
    formData.left = new FormAttachment(0, 5);
    formData.top = new FormAttachment(0, 5);
    // this.combo.setLayoutData(formData);

    formData = new FormData();
    formData.left = new FormAttachment(0, 5);
    formData.right = new FormAttachment(100, -5);
    formData.top = new FormAttachment(5);
    formData.bottom = new FormAttachment(100, -5);
    this.viewer.getTree().getParent().setLayoutData(formData);

    // initialize viewer
    this.initControls();

    // register listeners on combo box and viewer
    // this.combo.addSelectionListener(new ComboSelectionListener());
    this.viewer.addCheckStateListener(new ViewerCheckboxListener());

    // register listener on TSContainerManager
    // CodeCoverPlugin.getDefault().getTSContainerManager().addListener(this);
  }

  /*
   * methods to react on change of the list of known test session containers or on change of the active test
   * session container
   */

  /**
   * Fills the combo box with an entry for every test session container, sets the input of the viewer to the
   * activated test session container and disable the actions if there is no active test session container.
   */
  private void initControls() {
    ActiveTSContainerInfo activeTSCInfo =
      CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
    this.tscInfos =
      new LinkedList<TSContainerInfo>(CodeCoverPlugin.getDefault().getTSContainerManager()
        .getTestSessionContainers());

    // this.combo.removeAll(); // just to be safe
    // for(TSContainerInfo tscInfo : this.getVisTSCInfos()) {
    // this.combo.add(tscInfo.getName());
    // }
    // // adjust size of combo box
    // this.combo.pack(true);
    //
    // // set the active TSC and synchronize viewer with selection of combo box
    // if(!this.getVisTSCInfos().isEmpty()) {
    // if(activeTSCInfo != null) {
    // this.combo.select(this.getVisTSCInfos().indexOf(activeTSCInfo));
    // } else {
    // this.noTSCactive();
    // }
    this.setViewerInput(activeTSCInfo);
    // } else /* (if combo box is empty) */{
    // this.noTSCinCombo();
    // this.setViewerInput(null);
    // }
  }

  private final Set<CoverableItem> CreateCoverableItemSet(String Criterion) {
    if (this.wholeTestCases.size() == 0) {
      throw new IllegalArgumentException("testCases.size() == 0");
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

  private final void CalculateSuiteRedundancy(String Criterion) {
    Set<CoverableItem> coverableItemSet = CreateCoverableItemSet(Criterion);
    Set<CoverableItem> mainSet = new HashSet<CoverableItem>(coverableItemSet);
    Set<CoverableItem> otherTestSet = new HashSet<CoverableItem>();
    if (Criterion.compareTo("Condition") != 0) {
      for (int i = 0; i < this.wholeTestCases.size(); i++) {
        TestCase currentTestCase = this.wholeTestCases.get(i);
        Set<CoverableItem> currentCoveredSet = new HashSet<CoverableItem>(currentTestCase.getCoverageData().keySet());
        currentCoveredSet.retainAll(mainSet);
        int ts = currentCoveredSet.size();
        otherTestSet.clear();
        for (int a = 0; a < this.wholeTestCases.size(); a++) {
          if (a != i) {
            TestCase secondTestCase = this.wholeTestCases.get(a);
            Set<CoverableItem> secondSet = new HashSet<CoverableItem>(secondTestCase.getCoverageData().keySet());
            secondSet.retainAll(mainSet);
            otherTestSet.addAll(secondSet);
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

        if (Criterion == "Statement") {
          if (!SuiteR.isNaN()) {
            BigDecimal bd = new BigDecimal(SuiteR);
            bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
            this.wholeTestCases.get(i).setSatementRedundancy(bd.doubleValue());
          } else {
            this.wholeTestCases.get(i).setSatementRedundancy(SuiteR);
          }
          this.wholeTestCases.get(i).setSatementCoveredItem(ts);
        } else if (Criterion == "Branch") {
          if (!SuiteR.isNaN()) {
            BigDecimal bd = new BigDecimal(SuiteR);
            bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
            this.wholeTestCases.get(i).setBranchRedundancy(bd.doubleValue());
          } else {
            this.wholeTestCases.get(i).setBranchRedundancy(SuiteR);
          }
          this.wholeTestCases.get(i).setBranchCoveredItem(ts);
        } else if (Criterion == "Loop") {
          if (!SuiteR.isNaN()) {
            BigDecimal bd = new BigDecimal(SuiteR);
            bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
            this.wholeTestCases.get(i).setLoopRedundancy(bd.doubleValue());
          } else {
            this.wholeTestCases.get(i).setLoopRedundancy(SuiteR);
          }
          this.wholeTestCases.get(i).setLoopCoveredItem(ts);
        } else if (Criterion == "All") {
          if (!SuiteR.isNaN()) {
            BigDecimal bd = new BigDecimal(SuiteR);
            bd = bd.setScale(2, BigDecimal.ROUND_HALF_UP);
            this.wholeTestCases.get(i).setTotalRedundancy(bd.doubleValue());
          } else {
            this.wholeTestCases.get(i).setTotalRedundancy(SuiteR);
          }
          this.wholeTestCases.get(i).setTotalCoveredItem(ts);
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
          Set<BooleanAssignment> sharedAssignments = new HashSet<BooleanAssignment>(map1.getEvaluatedAssignments());
          Set<BooleanAssignment> otherAssignments = new HashSet<BooleanAssignment>();
          for (int a = 0; a < this.wholeTestCases.size(); a++) {
            if (a != i) {
              TestCase secondTestCase = this.wholeTestCases.get(a);
              Set<CoverableItem> secondSet = new HashSet<CoverableItem>(secondTestCase.getAssignmentsMap().keySet());
              secondSet.retainAll(mainSet);
              if (secondSet.contains(item)) {
                BooleanAssignmentMap map2 =
                  secondTestCase.getAssignmentsMap().get(item);
                otherAssignments.addAll(map2.getEvaluatedAssignments());
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
          this.wholeTestCases.get(i).setCondRedundancy(bd.doubleValue());
        } else {
          this.wholeTestCases.get(i).setCondRedundancy(SuiteR);
        }
        this.wholeTestCases.get(i).setCondCoveredItem(ts);
      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerAdded(org.codecover.eclipse.tscmanager.TSContainerManager.TSContainerInfo,
   *      int)
   */
  public void testSessionContainerAdded(final TSContainerInfo tscInfo, final int index) {
    ManualRedundancyView.runInUI(new Runnable() {

      @Override
	public void run() {
        // if(!ManualRedundancyView.this.combo.isDisposed()) {
        // // first remove no-TSCs-message
        // if(ManualRedundancyView.this.getVisTSCInfos().isEmpty()) {
        // ManualRedundancyView.this.combo.removeAll();
        // }
        // ManualRedundancyView.this.combo.add(tscInfo.getName(), index);
        // // add no-TSC-active-message to the end of the combo
        // if(ManualRedundancyView.this.getVisTSCInfos().isEmpty()) {
        // if(ManualRedundancyView.this.getVisTSCInfo() == null) {
        // noTSCactive();
        // } else {
        // ManualRedundancyView.this.showErrorInconsistency();
        // }
        // }
        // ManualRedundancyView.this.getVisTSCInfos().add(index, tscInfo);
        // ManualRedundancyView.this.combo.pack(true);
        // }
      }
    }, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(ActiveTSContainerInfo)
   */
  public void testSessionContainerActivated(final ActiveTSContainerInfo tscInfo) {
    ManualRedundancyView.runInUI(new Runnable() {

      @Override
	public void run() {
        if (!ManualRedundancyView.this.viewer.getControl().isDisposed()) {
          if (tscInfo != null) {
            int index = ManualRedundancyView.this.getVisTSCInfos().indexOf(tscInfo.getTSContainerInfo());
            if (index != -1) {
              // ManualRedundancyView.this.combo.select(index);
              /*
               * if previously there was no TSC active, the according message must be removed from the combo
               */
              if (ManualRedundancyView.this.getVisTSCInfo() == null) {
                // ManualRedundancyView.this.combo.remove(
                // ManualRedundancyView.this
                // .combo.getItemCount()-1);
                // ManualRedundancyView.this.combo.pack(true);
              }
            } else {
              ManualRedundancyView.this.showErrorInconsistency();
            }
          } else if (!ManualRedundancyView.this.getVisTSCInfos().isEmpty()) {
            ManualRedundancyView.this.noTSCactive();
          }

          ManualRedundancyView.this.setViewerInput(tscInfo);
        }
      }
      /*
       * the sync parameter must be true or else the enable-state of the save action can be set wrong, because
       * the view will ignore synchronizedStateChanged-events until the TSContainerInfo returned by
       * this.getVisTSCInfo is set correctly
       */
    }, true);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(ActiveTSContainerInfo,
   *      ChangeType, TestCase)
   */
  public void testCaseChanged(final ActiveTSContainerInfo tscInfo, final ChangeType changeType,
    final TestCase testCase) {
    ManualRedundancyView.runInUI(new Runnable() {

      @Override
	public void run() {
        final String[] updateProps =
          {RedundancyViewerFactory.UPDATE_PROPERTY_NAME, RedundancyViewerFactory.UPDATE_PROPERTY_COMMENT
          /*
           * date is omitted here, since the model doesn't allow changes of the date of test elements
           */};
        if (!ManualRedundancyView.this.viewer.getControl().isDisposed()) {
          if (tscInfo.getTestSessionContainer() == ManualRedundancyView.this.getVisTSC()) {
            if (changeType == ChangeType.CHANGE) {
              ManualRedundancyView.this.viewer.update(testCase, updateProps);
            } else if (changeType == ChangeType.ADD) {
              ManualRedundancyView.this.viewer.refresh(testCase.getTestSession(), false);
              ManualRedundancyView.this.viewer.reveal(testCase);
            } else if (changeType == ChangeType.REMOVE) {
              if (ManualRedundancyView.this.getVisTestCases().remove(testCase)) {
                ManualRedundancyView.this.viewer.setCheckedElements(ManualRedundancyView.this
                  .getVisTestCases().toArray());
              }
              ManualRedundancyView.this.viewer.refresh(testCase.getTestSession(), false);
            }
          }
        }
      }
    },
    /*
     * the sync parameter must be true or else the reveal method can cause trouble if the test case is already
     * deleted, when reveal is called
     */
    true);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(ActiveTSContainerInfo,
   *      ChangeType, TestSession)
   */
  public void testSessionChanged(final ActiveTSContainerInfo tscInfo, final ChangeType changeType,
    final TestSession testSession) {
    ManualRedundancyView.runInUI(new Runnable() {

      @Override
	public void run() {
        boolean testCaseRemoved;
        boolean curTestCaseRemoved;
        final String[] updateProps =
          {RedundancyViewerFactory.UPDATE_PROPERTY_NAME, RedundancyViewerFactory.UPDATE_PROPERTY_COMMENT
          /*
           * date is omitted here, since the model doesn't allow changes of the date of test elements
           */};
        if (!ManualRedundancyView.this.viewer.getControl().isDisposed()
          && tscInfo.getTestSessionContainer() == ManualRedundancyView.this.getVisTSC()) {
          if (changeType == ChangeType.CHANGE) {
            ManualRedundancyView.this.viewer.update(testSession, updateProps);
          } else if (changeType == ChangeType.ADD) {
            ManualRedundancyView.this.viewer.refresh(false);
          } else if (changeType == ChangeType.REMOVE) {
            testCaseRemoved = false;
            for (TestCase testCase : testSession.getTestCases()) {
              curTestCaseRemoved = ManualRedundancyView.this.getVisTestCases().remove(testCase);
              testCaseRemoved = testCaseRemoved || curTestCaseRemoved;
            }
            if (testCaseRemoved) {
              ManualRedundancyView.this.viewer.setCheckedElements(ManualRedundancyView.this.getVisTestCases()
                .toArray());
            }
            ManualRedundancyView.this.viewer.refresh(false);
          }
        }
      }
    }, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerChanged(ChangeType,
   *      ActiveTSContainerInfo)
   */
  public void testSessionContainerChanged(ChangeType changeType, ActiveTSContainerInfo tscInfo) {
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(ActiveTSContainerInfo)
   */
  public void testCasesActivated(final ActiveTSContainerInfo tscInfo) {
    ManualRedundancyView.runInUI(new Runnable() {

      @Override
	public void run() {
        if (!ManualRedundancyView.this.viewer.getControl().isDisposed()
          && tscInfo.getTestSessionContainer() == ManualRedundancyView.this.getVisTSC()) {
          if (!ManualRedundancyView.this.getVisTestCases().equals(tscInfo.getRedundantTestCases())) {
            ManualRedundancyView.this.getVisTestCases().clear();
            ManualRedundancyView.this.getVisTestCases().addAll(tscInfo.getRedundantTestCases());
            ManualRedundancyView.this.viewer.setCheckedElements(tscInfo.getRedundantTestCases().toArray());
          }
        }
      }
    }, false);
  }

  /*
   * (non-Javadoc)
   *
   * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#synchronizedStateChanged(TSContainerInfo,
   *      boolean)
   */
  public void synchronizedStateChanged(final TSContainerInfo tscInfo, final boolean isSynchronized) {
    runInUI(new Runnable() {

      @Override
	public void run() {
        TSContainerInfo visTSCInfo = ManualRedundancyView.this.getVisTSCInfo();
        if (visTSCInfo != null && visTSCInfo.equals(tscInfo)) {
          ManualRedundancyView.this.saveTSC.setEnabled(!isSynchronized);
        }
      }
    }, false);
  }

  private static void runInUI(Runnable runnable, boolean sync) {
    if (Display.getCurrent() != null) {
      runnable.run();
    } else {
      if (sync) {
        Display.getDefault().syncExec(runnable);
      } else {
        Display.getDefault().asyncExec(runnable);
      }
    }
  }

  private final class ViewerCheckboxListener
    implements ICheckStateListener {

    /**
     * Handles changes in states of checkboxes (checked, unchecked) of the viewer.
     *
     * @param event the {@link CheckStateChangedEvent}
     */
    @Override
	public void checkStateChanged(CheckStateChangedEvent event) {
      if (event.getElement() instanceof TestSession) {
        TestSession testSession = (TestSession) event.getElement();
        if (event.getChecked() == true) {
          ManualRedundancyView.this.getVisTestCases().addAll(testSession.getTestCases());
        } else if (event.getChecked() == false) {
          ManualRedundancyView.this.getVisTestCases().removeAll(testSession.getTestCases());
        }
      } else if (event.getElement() instanceof TestCase) {
        TestCase testCase = (TestCase) event.getElement();
        if (event.getChecked() == true) {
          ManualRedundancyView.this.getVisTestCases().add(testCase);
        } else {
          ManualRedundancyView.this.getVisTestCases().remove(testCase);
        }
      }
      ActiveTSContainerInfo activeTSContainer =
        CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
      CodeCoverPlugin.getDefault().getTSContainerManager().setRedundantTestCases(
        activeTSContainer.getActiveTestCases(), ManualRedundancyView.this.getVisTestCases());
    }
  }

  /**
   * Sets the state of the checkboxes of the active <code>TestCase</code>s of the active
   * <code>TestSessionContainer</code> to checked.
   */
  private void checkRedundantTestCases() {
    if (this.getVisTSC() == null) {
      return;
    }
    // check test cases
    for (TestCase testCase : this.getVisTestCases()) {
      this.viewer.setChecked(testCase, true);
    }
    // check test sessions
    for (TestSession testSession : this.getVisTSC().getTestSessions()) {
      if (isCheckboxStateOfSubtree(true, testSession)) {
        this.viewer.setChecked(testSession, true);
      } else if (isCheckboxStateOfSubtree(false, testSession)) {
        this.viewer.setChecked(testSession, false);
      } else {
        this.viewer.setGrayChecked(testSession, true);
      }
    }
  }

  /**
   * Checks if the states of the checkboxes of all <code>TestCase</code>s of a given
   * <code>TestSession</code> match a given state (checked or unchecked).
   *
   * @param state the state to check the checkboxes for
   * @param testSession the <code>TestSession</code> which <code>TestCase</code>s are checked for the
   *        given state of their checkboxes
   * @return <code>true</code>, if the states of the checkboxes of all <code>TestCase</code>s of the
   *         given <code>TestSession</code> match the given state (checked or unchecked).
   */
  private boolean isCheckboxStateOfSubtree(boolean state, TestSession testSession) {
    if (state == true && testSession.getTestCases().isEmpty()) {
      return false;
    }
    for (TestCase testCase : testSession.getTestCases()) {
      if (this.viewer.getChecked(testCase) != state) {
        return false;
      }
    }
    return true;
  }

  /*
   * methods and class to handle the combo box (which lists the known test session containers)
   */

  /**
   * Makes the combo visualize that no test session container is active.
   */
  private void noTSCactive() {
    // empty
  }

  private void showErrorInconsistency() {
    MessageDialog.openError(this.getSite().getShell(), DIALOG_ERROR_INCONSISTENCY_TITLE,
      DIALOG_ERROR_INCONSISTENCY_MSG);
    ManualRedundancyView.this.logger.fatal("User interface and data model are inconsistent!"); //$NON-NLS-1$

  }

  /*
   * methods and classes to create actions and hook them to context menus, toolbars and the tree viewer
   */

  private void makeActions() {
    /*
     * save was moved to its own class since it could be used in other views as well
     */
    this.saveTSC = new SaveActiveTSContainerAction();
    this.deleteTSC = this.makeTSCDeleteAction();
    this.selectAll = this.makeSelectAllAction();
    this.activateAll = this.makeActivateAllAction();
    this.deactivateAll = this.makeDeactivateAllAction();
    this.merge = this.makeMergeAction();
    this.deleteToolBar = this.makeTestElementDeleteAction(false);
    this.deleteContextMenu = this.makeTestElementDeleteAction(true);
    this.propertiesAction = this.makePropertiesAction();
    this.showActiveTestCases = this.makeActiveTestCasesDialogAction();
  }

  private void fillActionMenus() {
    IActionBars bars = this.getViewSite().getActionBars();
    // fill toolbar of view
    bars.getToolBarManager().add(this.saveTSC);
    bars.getToolBarManager().add(this.deleteTSC);
    bars.getToolBarManager().add(new Separator());
    bars.getToolBarManager().add(this.merge);
    bars.getToolBarManager().add(this.deleteToolBar);
    // fill pulldown menu of view
    bars.getMenuManager().add(this.saveTSC);
    bars.getMenuManager().add(this.deleteTSC);
    if (CodeCoverPlugin.getDefault().getLogLevel() == LogLevel.DEBUG) {
      bars.getMenuManager().add(this.showActiveTestCases);
    }
    bars.getMenuManager().add(new Separator());
    bars.getMenuManager().add(this.merge);
    bars.getMenuManager().add(this.deleteToolBar);
    // fill context menu
    MenuManager menuMgr = new MenuManager("#PopupMenu"); //$NON-NLS-1$
    menuMgr.setRemoveAllWhenShown(true);
    menuMgr.addMenuListener(new IMenuListener() {

      @Override
	public void menuAboutToShow(IMenuManager manager) {
        manager.add(ManualRedundancyView.this.selectAll);
        manager.add(new Separator());
        manager.add(ManualRedundancyView.this.activateAll);
        manager.add(ManualRedundancyView.this.deactivateAll);
        manager.add(ManualRedundancyView.this.deleteContextMenu);
        manager.add(ManualRedundancyView.this.propertiesAction);
        // other plugins can contribute their actions here
        manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
      }
    });
    Menu menu = menuMgr.createContextMenu(this.viewer.getControl());
    this.viewer.getControl().setMenu(menu);
    this.getSite().registerContextMenu(menuMgr, this.viewer);
  }

  private void hookDoubleClickAction() {
    this.viewer.addDoubleClickListener(new IDoubleClickListener() {

      @Override
	public void doubleClick(DoubleClickEvent event) {
        ManualRedundancyView.this.propertiesAction.run();
      }
    });
  }

  private Action makeTSCDeleteAction() {
    return new TSCDeleteAction();
  }

  private class TSCDeleteAction
    extends Action
    implements IMenuCreator {

    private Menu menu;

    TSCDeleteAction() {
      super(TSC_DELETE_ACTION_TEXT, IAction.AS_DROP_DOWN_MENU);
      this.setToolTipText(TSC_DELETE_ACTION_TOOLTIP);
      this.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
        CodeCoverPlugin.Image.SESSION_CONTAINER_DELETE.getPath()));
      this.setMenuCreator(this);
      this.menu = null;
    }

    /**
     * Deletes the active test session container.
     */
    @Override
    public void run() {
      TSContainerInfo activeTSCInfo = ManualRedundancyView.this.getVisTSCInfo();

      if (activeTSCInfo == null) {
        return;
      }

      boolean deleteTSC =
        MessageDialog.openQuestion(ManualRedundancyView.this.viewer.getControl().getShell(),
          TSC_DELETE_DIALOG_TITLE, String.format(TSC_DELETE_DIALOG_MSG, activeTSCInfo.getName()));
      if (deleteTSC) {
        try {
          CodeCoverPlugin.getDefault().getTSContainerManager().deleteTestSessionContainer(activeTSCInfo,
            new NullProgressMonitor());
        } catch (CoreException e) {
          ErrorDialog.openError(CodeCoverPlugin.getDefault().getWorkbench().getDisplay().getActiveShell(),
            TSC_DELETE_DIALOG_ERROR_TITLE, null, new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID,
              IStatus.OK, TSC_DELETE_DIALOG_ERROR_MSG, e));
        } catch (CancelException e) {
          ManualRedundancyView.this.logger.warning("Canceled deleting of" + //$NON-NLS-1$
            " test session container.", e); //$NON-NLS-1$
        }
      }
    }

    @Override
	public Menu getMenu(Control parent) {
      ActionContributionItem item;
      if (this.menu != null && this.menu.getParent().equals(parent)) {
        return this.menu;
      } else {
        if (this.menu != null) {
          this.menu.dispose();
        }
        this.menu = new Menu(parent);
        item = new ActionContributionItem(new MultipleTSCsDeleteAction());
        item.fill(this.menu, -1);
        return this.menu;
      }
    }

    @Override
	public Menu getMenu(Menu parent) {
      return null;
    }

    @Override
	public void dispose() {
      if (this.menu != null) {
        this.menu.dispose();
        this.menu = null;
      }
    }
  };

  private class MultipleTSCsDeleteAction
    extends Action {

    MultipleTSCsDeleteAction() {
      this.setText(TSC_DELETE_MULTIPLE_ACTION_TEXT);
      this.setToolTipText(TSC_DELETE_MULTIPLE_ACTION_TOOLTIP);
    }

    @Override
    public void run() {
      (new DeleteTSCsSelectDialog(ManualRedundancyView.this.getSite().getShell())).open();
    }
  }

  private Action makeTestElementDeleteAction(boolean forContextMenu) {
    return new TestElementDeleteAction(this.getSite().getShell(), this.viewer, forContextMenu);
  }

  /**
   * An action to delete test sessions or test cases selected in the (tree) viewer of the Test Sessions view.
   */
  private final class TestElementDeleteAction
    extends Action
    implements ISelectionChangedListener {

    private Shell shell;

    private CheckboxTreeViewer viewerOfTestSessionsView;

    private boolean inContextMenu;

    TestElementDeleteAction(Shell shell, CheckboxTreeViewer viewer, boolean inContextMenu) {
      this.shell = shell;
      this.viewerOfTestSessionsView = viewer;
      this.inContextMenu = inContextMenu;

      if (this.inContextMenu) {
        this.setText(ELEMENTS_DELETE_ACTION_TEXT_CONTEXTMENU);
        this.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
          ISharedImages.IMG_TOOL_DELETE));
      } else {
        this.setText(ELEMENTS_DELETE_ACTION_TEXT_TOOLBAR);
        this.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
          CodeCoverPlugin.Image.ELEMENTS_DELETE.getPath()));
      }
      this.setToolTipText(ELEMENTS_DELETE_ACTION_TOOLTIP);
    }

    @Override
    public void run() {
      IStructuredSelection sel = null;

      if (ManualRedundancyView.this.getVisTSC() == null) {
        return;
      }

      if (this.viewerOfTestSessionsView.getSelection() instanceof IStructuredSelection) {
        sel = (IStructuredSelection) this.viewerOfTestSessionsView.getSelection();
      }

      if (this.inContextMenu && sel != null && sel.size() == 1) {
        Object[] elementToDelete = sel.toArray();
        DeleteTestElementsConfirmDialog confirmDialog =
          new DeleteTestElementsConfirmDialog(this.shell, elementToDelete);
        confirmDialog.setBlockOnOpen(true);
        if (confirmDialog.open() == DeleteTestElementsConfirmDialog.YES_BUTTON_INDEX) {
          try {
            CodeCoverPlugin.getDefault().getTSContainerManager().setActiveTSContainer(
              ManualRedundancyView.this.getVisTSCInfo(), new DeleteTestElementsRunnable(elementToDelete),
              null);
          } catch (CancelException e) {
            ManualRedundancyView.this.logger.warning("User canceled deletion of" + //$NON-NLS-1$
              " test sessions/test cases", e); //$NON-NLS-1$
          } catch (Exception e) {
            ErrorDialog.openError(CodeCoverPlugin.getDefault().getWorkbench().getDisplay().getActiveShell(),
              DIALOG_ERROR_ELEMENT_DELETE_TITLE, null, new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID,
                IStatus.OK, DIALOG_ERROR_ELEMENT_DELETE_MSG, e));
            ManualRedundancyView.this.logger.error("Error while deleting" + //$NON-NLS-1$
              " test session/test case", e); //$NON-NLS-1$
          } catch (OutOfMemoryError e) {
            ErrorDialog.openError(CodeCoverPlugin.getDefault().getWorkbench().getDisplay().getActiveShell(),
              DIALOG_ERROR_ELEMENT_DELETE_TITLE, null, new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID,
                IStatus.OK, DIALOG_ERROR_ELEMENT_DELETE_MSG, e));
            ManualRedundancyView.this.logger.error("Error while deleting" + //$NON-NLS-1$
              " test session/test case", //$NON-NLS-1$
              new InvocationTargetException(e));
          }
        }
      } else {
        (new DeleteTestElementsSelectDialog(this.shell, sel, ManualRedundancyView.this.getVisTSCInfo(),
          ManualRedundancyView.this.getVisTSC(), ManualRedundancyView.this.logger)).open();
      }
    }

    @Override
	public void selectionChanged(SelectionChangedEvent e) {
      IStructuredSelection sel;
      if (e.getSelection().isEmpty()) {
        this.setEnabled(false);
        return;
      }
      if (!(e.getSelection() instanceof IStructuredSelection)) {
        return;
      }
      sel = (IStructuredSelection) e.getSelection();
      if (sel.getFirstElement() instanceof TestSession || sel.getFirstElement() instanceof TestCase) {
        this.setEnabled(true);
      } else {
        this.setEnabled(false);
      }
    }

  }

  private Action makeMergeAction() {
    Action action = new Action() {

      @Override
      public void run() {
        MergeWizard mergeWizard;
        WizardDialog wizardDialog;
        if (ManualRedundancyView.this.getVisTSC() == null) {
          return;
        }
        mergeWizard =
          new MergeWizard(ManualRedundancyView.this.getVisTSC(),
            (IStructuredSelection) ManualRedundancyView.this.viewer.getSelection());
        // Instantiates the wizard container with the wizard and opens it
        wizardDialog = new WizardDialog(ManualRedundancyView.this.getSite().getShell(), mergeWizard);
        wizardDialog.create();
        wizardDialog.open();
      }
    };
    action.setText(MERGE_ACTION_TEXT);
    action.setToolTipText(MERGE_ACTION_TOOLTIP);
    action.setImageDescriptor(CodeCoverPlugin.getDefault().getImageRegistry().getDescriptor(
      CodeCoverPlugin.Image.ELEMENTS_MERGE.getPath()));

    return action;
  }

  private Action makeSelectAllAction() {
    Action action = new Action() {

      @Override
      public void run() {
        List<Object> allTSandTC = new ArrayList<Object>();
        // add all test sessions
        allTSandTC.addAll(ManualRedundancyView.this.getVisTSC().getTestSessions());
        // add all test cases
        for (TestSession testSession : ManualRedundancyView.this.getVisTSC().getTestSessions()) {
          allTSandTC.addAll(testSession.getTestCases());
        }
        ManualRedundancyView.this.viewer.setSelection(new StructuredSelection(allTSandTC));
      }
    };
    action.setText(SELECT_ALL_ACTION_TEXT);
    action.setToolTipText(SELECT_ALL_ACTION_TOOLTIP);

    return action;
  }

  private Action makeActivateAllAction() {
    Action action = new Action() {

      @Override
      public void run() {
        for (TestSession testSession : ManualRedundancyView.this.getVisTSC().getTestSessions()) {
          ManualRedundancyView.this.getVisTestCases().addAll(testSession.getTestCases());
        }
        ManualRedundancyView.this.viewer.setAllChecked(true);

        ActiveTSContainerInfo activeTSContainer =
          CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
        CodeCoverPlugin.getDefault().getTSContainerManager().setRedundantTestCases(
          activeTSContainer.getActiveTestCases(), ManualRedundancyView.this.getVisTestCases());
      }
    };
    action.setText(ACTIVATE_ALL_ACTION_TEXT);
    action.setToolTipText(ACTIVATE_ALL_ACTION_TOOLTIP);

    return action;
  }

  private Action makeDeactivateAllAction() {
    Action action = new Action() {

      @Override
      public void run() {
        ManualRedundancyView.this.getVisTestCases().clear();
        ManualRedundancyView.this.viewer.setAllChecked(false);

        ActiveTSContainerInfo activeTSContainer =
          CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
        CodeCoverPlugin.getDefault().getTSContainerManager().setRedundantTestCases(
          activeTSContainer.getActiveTestCases(), ManualRedundancyView.this.getVisTestCases());
      }
    };
    action.setText(DEACTIVATE_ALL_ACTION_TEXT);
    action.setToolTipText(DEACTIVATE_ALL_ACTION_TOOLTIP);

    return action;
  }

  private Action makePropertiesAction() {
    return new TestElementPropertiesAction(this.getSite().getShell(), this.viewer);
  }

  /**
   * An action which allows editing the properties of a test session or test case selected in the (tree)
   * viewer of the Test Sessions view.
   */
  private final class TestElementPropertiesAction
    extends Action
    implements ISelectionChangedListener {

    private Shell shell;

    private CheckboxTreeViewer viewer;

    TestElementPropertiesAction(Shell shell, CheckboxTreeViewer viewer) {
      this.shell = shell;
      this.viewer = viewer;
      this.setText(PROPERTIES_ACTION_TEXT);
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run() {
      ISelection sel = this.viewer.getSelection();
      if (!(sel instanceof IStructuredSelection) || ((IStructuredSelection) sel).isEmpty()) {
        return;
      }

      Object selectedObject = ((IStructuredSelection) sel).getFirstElement();
      (new TestElementPropertiesDialog(this.shell, selectedObject, ManualRedundancyView.this.getVisTSCInfo(),
        ManualRedundancyView.this.logger)).open();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.viewers.ISelectionChangedListener#selectionChanged(org.eclipse.jface.viewers.SelectionChangedEvent)
     */
    @Override
	public void selectionChanged(SelectionChangedEvent e) {
      IStructuredSelection sel;
      if (e.getSelection().isEmpty()) {
        this.setEnabled(false);
        return;
      }
      if (!(e.getSelection() instanceof IStructuredSelection)) {
        return;
      }
      sel = (IStructuredSelection) e.getSelection();
      if (sel.getFirstElement() instanceof TestSession || sel.getFirstElement() instanceof TestCase) {
        this.setEnabled(true);
      } else {
        this.setEnabled(false);
      }
    }

  }

  /**
   * Generates an action which opens a dialog which lists all active test cases as reported by the
   * TSContainerManager. This action is used for debugging purposes.
   *
   * @return an action which opens a dialog which lists all active test cases as reported by the
   *         TSContainerManager.
   */
  private Action makeActiveTestCasesDialogAction() {
    Action action = new Action() {

      @Override
      public void run() {
        ActiveTSContainerInfo activeTSCInfo =
          CodeCoverPlugin.getDefault().getTSContainerManager().getActiveTSContainer();
        StringBuilder msg = new StringBuilder("The following listing shows the active" + //$NON-NLS-1$
          " test cases\n" + //$NON-NLS-1$
          "as reported by the TSContainerManager." + //$NON-NLS-1$
          "\n\n"); //$NON-NLS-1$
        if (activeTSCInfo != null) {
          msg.append("Active Test Session Container:\n" //$NON-NLS-1$
            + activeTSCInfo.getName() + "\n\n" //$NON-NLS-1$
            + "Active Test Cases:\n"); //$NON-NLS-1$

          boolean isEmpty = true;
          for (TestCase testCase : activeTSCInfo.getRedundantTestCases()) {
            msg.append(testCase.getName() + " (" //$NON-NLS-1$
              + testCase.getTestSession().getName() + ") \n"); //$NON-NLS-1$
            isEmpty = false;
          }
          if (isEmpty) {
            msg.append("No active test cases." + //$NON-NLS-1$
              " (Select some!)"); //$NON-NLS-1$
          }
        } else {
          msg.append("No active test session" + //$NON-NLS-1$
            " container. (Activate one!)"); //$NON-NLS-1$
        }
        MessageDialog.openInformation(ManualRedundancyView.this.viewer.getControl().getShell(),
          "Active Test Cases", //$NON-NLS-1$
          msg.toString());
      }
    };
    action.setText("Show active test cases"); //$NON-NLS-1$
    action.setToolTipText("Show active test cases of active" + //$NON-NLS-1$
      " test session container"); //$NON-NLS-1$
    action.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages().getImageDescriptor(
      ISharedImages.IMG_OBJS_INFO_TSK));
    return action;
  }

  /*
   * methods to store and restore the expanded elements of the tree viewer
   */

  private void saveExpandedElements(TSContainerInfo tscInfo) {
    if (tscInfo != null) {
      this.expandedElements.remove(tscInfo);
      this.expandedElements.put(tscInfo, generateTestSessionNamesList(this.viewer.getExpandedElements()));
    }
  }

  private static Set<String> generateTestSessionNamesList(Object[] objs) {
    Set<String> testSessionNames = new HashSet<String>();
    for (Object o : objs) {
      if (o instanceof TestSession) {
        testSessionNames.add(((TestSession) o).getName());
      }
    }
    return testSessionNames;
  }

  private static Object[] fetchTestSessions(Set<String> testSessionNames, TestSessionContainer tsc) {
    Set<TestSession> testSessions = new HashSet<TestSession>();
    TestSession testSession;
    if (testSessionNames != null) {
      for (String testSessionName : testSessionNames) {
        testSession = tsc.getTestSessionWithName(testSessionName);
        if (testSession != null) {
          testSessions.add(testSession);
        }
      }
    }
    return testSessions.toArray();
  }

  private void restoreExpandedElements(ActiveTSContainerInfo activeTSCInfo) {
    Object[] tscExpElems;
    if (activeTSCInfo != null) {
      tscExpElems =
        fetchTestSessions(this.expandedElements.get(activeTSCInfo), activeTSCInfo.getTestSessionContainer());
      if (tscExpElems != null) {
        this.viewer.setExpandedElements(tscExpElems);
      }
    }
  }

  /*
   * (non-Javadoc)
   *
   * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
   */
  @Override
  public void setFocus() {
    this.viewer.getControl().setFocus();
  }

}
