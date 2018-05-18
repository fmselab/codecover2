
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
import java.awt.Font;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.Shape;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.LinkedList;
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
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.metrics.coverage.BranchCoverage;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.metrics.coverage.LoopCoverage;
import org.codecover.metrics.coverage.StatementCoverage;
import org.codecover.metrics.coverage.TermCoverage;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicStatement;
import org.codecover.model.mast.Branch;
import org.codecover.model.mast.ConditionalStatement;
import org.codecover.model.mast.CoverableItem;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.LoopingStatement;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.mast.Statement;
import org.codecover.model.mast.StatementSequence;
import org.codecover.model.utils.ChangeType;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuCreator;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IViewSite;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.swt.layout.FillLayout;

import edu.uci.ics.jung.algorithms.layout.GraphElementAccessor;
import edu.uci.ics.jung.algorithms.layout.Layout;
import edu.uci.ics.jung.graph.SparseMultigraph;
import edu.uci.ics.jung.graph.util.EdgeType;
import edu.uci.ics.jung.visualization.ScreenDevice;
import edu.uci.ics.jung.visualization.control.CrossoverScalingControl;
import edu.uci.ics.jung.visualization.control.DefaultModalGraphMouse;
import edu.uci.ics.jung.visualization.control.ScalingControl;
import edu.uci.ics.jung.visualization.decorators.EdgeShape;
import edu.uci.ics.jung.visualization.event.MouseEvent;
import edu.uci.ics.jung.visualization.event.MouseListener;
import edu.uci.ics.jung.visualization.renderers.Renderer.VertexLabel.Position;
import edu.uci.ics.jung.visualization.swt.GraphZoomScrollPane;
import edu.uci.ics.jung.visualization.swt.VisualizationComposite;

/**
 * This {@link CoverageGraphView} is a view in eclipse. It provides the user with
 * the opportunity to draw the coverage graph which shows how various items of 
 * the SUT are covered by each test case.
 * 
 * Project supervisor: Vahid Garousi (http://www.ucalgary.ca/~vgarousi/)
 * Software Quality Engineering Research Group (SoftQual)
 * Department of Electrical and Computer Engineering
 * Schulich School of Engineering
 * University of Calgary, Alberta, Canada
 * http://www.softqual.ucalgary.ca
 * @author Negar Koochakzadeh
 * @version 1.0 
 */
public class CoverageGraphView extends ViewPart {
	LocationList locList;
	Location loc;
	HierarchyLevel topLevel;
	String StContent;
	ActiveTSContainerInfo activeTSContainer;
	TestSessionContainer tsc;
	Statement st;
	
    private static final String TAG_SELECTED = "Selected"; //$NON-NLS-1$
    private final List<IAction> correlationActions;
    private final List<IAction> SUTLevels;   
    private final List<IAction> TestLevels;
    private final List<IAction> MouseStyle;
    private final List<IAction> Options;
    private final List<TestCase> selectedTestCases;
    private IMemento memento;
    private SashForm sashForm; 
    private boolean calculationPending = false;
    private final Object lock = new Object();
    private String ActiveCriterion;
    private String ActiveSUTLevel;
    private String ActiveTestLevel;
    private String SelectedMouseStyle;
    private Boolean SelectedOnlyCovered;
    private Boolean CompleteName;
    private GraphComposite graphComposite;
    private final TestCaseComparator testCaseComparator = new TestCaseComparator();   
    private static final String LABEL_CHOOSE_COVERAGE_CRITERIA = Messages
    .getString("CoverageGraphView.COVERAGE_CRITERIA"); //$NON-NLS-1$  
    private static final String LABEL_CHOOSE_SUT_LEVEL = Messages
    .getString("CoverageGraphView.SUT_LEVEL"); //$NON-NLS-1$   
    private static final String LABEL_CHOOSE_MOUSE_STYLE = Messages
    .getString("CoverageGraphView.MOUSE_STYLE"); //$NON-NLS-1$   
    private static final String LABEL_CHOOSE_OPTIONS = Messages
    .getString("CoverageGraphView.OPTIONS"); //$NON-NLS-1$   
    private static final String LABEL_CHOOSE_TEST_LEVEL = Messages
    .getString("CoverageGraphView.TEST_LEVEL"); //$NON-NLS-1$
    /**
     * Constructor. 
     */
    public CoverageGraphView() {
    	
    	SelectedOnlyCovered = false;
    	CompleteName = false;
    	this.SelectedMouseStyle = "Picking";
    	this.ActiveCriterion = "Statement";
    	this.ActiveSUTLevel = "Method";
    	this.ActiveTestLevel = "Method";
    	this.selectedTestCases = new Vector<TestCase>();
        this.correlationActions = new Vector<IAction>();
        this.TestLevels = new Vector<IAction>();
        this.SUTLevels = new Vector<IAction>();
        this.MouseStyle = new Vector<IAction>();
        this.Options = new Vector<IAction>();
        
        activeTSContainer = CodeCoverPlugin.getDefault()
        .getTSContainerManager().getActiveTSContainer();        
        if (!activeTSContainer.equals(null)) {
        	tsc = activeTSContainer.getTestSessionContainer();
        	selectedTestCases.addAll(sortTestCases(activeTSContainer
        			.getActiveTestCases()));
        	topLevel = activeTSContainer.getTestSessionContainer().getCode();
        }
        CodeCoverPlugin.getDefault().getTSContainerManager().addListener(
                new TSManagerListener());
    }
    private static enum Type {
        /**
         * Constant for projects.
         */
        PROJECT     (CoverageGraphView.Type.DEFAULT_PACKAGE_NAME),
        /**
         * Constant for packages.
         */
        PACKAGE     (CoverageGraphView.Type.PACKAGE_NAME),
        /**
         * Constant for classes, interfaces and enums.
         */
        CLASS       (CoverageGraphView.Type.CLASS_NAME),
        /**
         * Constant for methods.
         */
        METHOD      (CoverageGraphView.Type.METHOD_NAME);

        /*
         * The following constants are the internal names of all types.
         */
        private static final String DEFAULT_PACKAGE_NAME
                 = "default package";                              //$NON-NLS-1$
        private static final String PACKAGE_NAME = "package";      //$NON-NLS-1$
        private static final String CLASS_NAME = "class";          //$NON-NLS-1$
        private static final String INTERFACE_NAME = "interface";  //$NON-NLS-1$
        private static final String ENUM_NAME = "enum";            //$NON-NLS-1$
        private static final String ANNOTATION_NAME = "@interface";//$NON-NLS-1$
        private static final String METHOD_NAME = "method";        //$NON-NLS-1$

        private final String internalName;

        private Type(String internalName) {
            this.internalName = internalName;
        }

        /**
         * Returns the internal name of the corresponding type of a
         * <code>HierarchyLevel</code>
         * (see <code>HierarchyLevelType.getInternalName()</code>).
         * Java default packages are displayed as the project nodes, which are
         * always on top-level.
         * The name for
         * the {@link #CLASS} type is returned as &quot;class&quot; although it
         * subsumes classes, interfaces and enums.
         * 
         * @return  the internal name of the <code>HierarchyLevel</code>
         */
        public String getName() {
            return this.internalName;
        }

        /**
         * Returns the <code>Type</code> which matches the internal name
         * of the given <code>HierarchyLevel</code>.
         * 
         * @param hLev  the <code>HierarchyLevel</code>
         * 
         * @return  the <code>Type</code> which matches the internal name of the
         *          given <code>HierarchyLevel</code>
         */
        public static Type typeOf(HierarchyLevel hLev) {
            return Type.typeOf(hLev.getType().getInternalName());
        }

        /**
         * Returns the <code>Type</code> which matches the given internal name
         * of a <code>HierarchyLevel</code>.
         * 
         * @param name  the name
         * 
         * @return  the <code>Type</code> which matches the given internal name
         *          of a <code>HierarchyLevel</code>
         */
        public static Type typeOf(String name) {
            if(name.equals(Type.INTERFACE_NAME)
                    || name.equals(Type.ENUM_NAME)
                    || name.equals(Type.ANNOTATION_NAME)) {
                return Type.CLASS;
            }
            for(Type type : Type.values()) {
                if(type.getName().equals(name)) {
                    return type;
                }
            }
            return null;
        }
    }
    private List<HierarchyLevel> oldestChildrenOfType(Type rootType,
            HierarchyLevel child) {
        List<HierarchyLevel> children = new LinkedList<HierarchyLevel>();
        if(Type.typeOf(child) == rootType) {
            children.add(child);
            return children;
        }
        for(HierarchyLevel hLev : child.getChildren()) {
            children.addAll(oldestChildrenOfType(rootType, hLev));
        }
        return children;
    }
    protected static boolean checkTestCases(List<TestCase> testCases) {
        /* Check, if all the test cases share the same test session container */
        for (int i = 0; i < testCases.size() - 1; i++) {
            if (!testCases.get(i).getTestSession().getTestSessionContainer()
                    .equals(
                            testCases.get(i + 1).getTestSession()
                                    .getTestSessionContainer())) {
                return false;
            }
        }
        return true;
    }
    protected void showHierarchyLevelInEditor(HierarchyLevel hLev) {
    	ActiveTSContainerInfo activeTSContainer = CodeCoverPlugin.getDefault()
        .getTSContainerManager().getActiveTSContainer();
        TestSessionContainer tsc;
        tsc = activeTSContainer.getTestSessionContainer();

        /* open hLev in a text editor with highlighting */
        ITextEditor editor = EclipseMASTLinkage.openClassInEditor(hLev, tsc);

        if (editor == null) {
            /* failed: no suitable Resource can be opened */
        } else {
            /* show hLev in the Editor */
            EclipseMASTLinkage.showInEditor(editor,loc);
        }
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
    	this.Options.addAll(createOptionsActions());
    	this.MouseStyle.addAll(createMouseStyleActions());
        this.correlationActions.addAll(createCorrelationActions());
        this.SUTLevels.addAll(createSUTLevelsActions());
        this.TestLevels.addAll(createTestLevelsActions());

        initializeToolBar();

        org.eclipse.swt.graphics.Point size = new Point(1000, 500);
        this.sashForm = new SashForm(parent, SWT.HORIZONTAL);     
        graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        if(graphComposite.getLayout() != null){
        	graphComposite.setLayout(new GridLayout(2, false));
        	graphComposite.setLayout(new FillLayout());
        }
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
        IToolBarManager toolBarManager = getViewSite().getActionBars()
                .getToolBarManager();
        toolBarManager.add(createActionChooseMetric());
        toolBarManager.add(createActionChooseSUTLevel());
        toolBarManager.add(createActionChooseTestLevel());
        toolBarManager.add(createActionChooseMouseStyle());
        toolBarManager.add(createActionChooseOption());
    }

    private final IAction createActionChooseMetric() {
        IAction chooseMetricAction = new Action(
                LABEL_CHOOSE_COVERAGE_CRITERIA, IAction.AS_DROP_DOWN_MENU) {
            @Override
            public void run() {
//                onCalculateCorrelation();
            }
        };
        chooseMetricAction
                .setDescription(LABEL_CHOOSE_COVERAGE_CRITERIA);
        chooseMetricAction.setToolTipText(LABEL_CHOOSE_COVERAGE_CRITERIA);
        chooseMetricAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.COVERAGE_CRITERIA.getPath()));
        chooseMetricAction.setMenuCreator(new MetricMenuCreator());

        return chooseMetricAction;
    }
    private final IAction createActionChooseSUTLevel() {
        IAction chooseSUTLevelAction = new Action(
        		LABEL_CHOOSE_SUT_LEVEL, IAction.AS_DROP_DOWN_MENU) {
        	@Override
            public void run() {
//                onCalculateCorrelation();
            }
        };
        chooseSUTLevelAction
                .setDescription(LABEL_CHOOSE_SUT_LEVEL);
        chooseSUTLevelAction.setToolTipText(LABEL_CHOOSE_SUT_LEVEL);
        chooseSUTLevelAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.SUT_LEVEL.getPath()));
        chooseSUTLevelAction.setMenuCreator(new SUTLevelMenuCreator());

        return chooseSUTLevelAction;
    }
    private final IAction createActionChooseTestLevel() {
        IAction chooseTestLevelAction = new Action(
        		LABEL_CHOOSE_TEST_LEVEL, IAction.AS_DROP_DOWN_MENU) {
            @Override
            public void run() {
//                onCalculateCorrelation();
            }
        };
        chooseTestLevelAction
                .setDescription(LABEL_CHOOSE_TEST_LEVEL);
        chooseTestLevelAction.setToolTipText(LABEL_CHOOSE_TEST_LEVEL);
        chooseTestLevelAction.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.TEST_LEVEL.getPath()));
        chooseTestLevelAction.setMenuCreator(new TestLevelMenuCreator());

        return chooseTestLevelAction;
    }
    private final IAction createActionChooseMouseStyle() {
        IAction chooseMouseStyle = new Action(
        		LABEL_CHOOSE_MOUSE_STYLE, IAction.AS_DROP_DOWN_MENU) {
            @Override
            public void run() {
//                onCalculateCorrelation();
            }
        };
        chooseMouseStyle
                .setDescription(LABEL_CHOOSE_MOUSE_STYLE);
        chooseMouseStyle.setToolTipText(LABEL_CHOOSE_MOUSE_STYLE);
        chooseMouseStyle.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.MOUSE_STYLE.getPath()));
        chooseMouseStyle.setMenuCreator(new MouseStyleMenuCreator());

        return chooseMouseStyle;
    }
    private final IAction createActionChooseOption() {
        IAction chooseMouseStyle = new Action(
        		LABEL_CHOOSE_OPTIONS, IAction.AS_DROP_DOWN_MENU) {
            @Override
            public void run() {
//                onCalculateCorrelation();
            }
        };
        chooseMouseStyle
                .setDescription(LABEL_CHOOSE_OPTIONS);
        chooseMouseStyle.setToolTipText(LABEL_CHOOSE_OPTIONS);
        chooseMouseStyle.setImageDescriptor(CodeCoverPlugin.getDefault()
                .getImageRegistry().getDescriptor(
                        CodeCoverPlugin.Image.OPTIONS.getPath()));
        chooseMouseStyle.setMenuCreator(new OptionMenuCreator());

        return chooseMouseStyle;
    }
    private final void onChooseShowAll(Boolean ShowOnlyCovered) {
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	SelectedOnlyCovered = ShowOnlyCovered;
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true);
    }
    private final void onChooseCompleteName(Boolean CompleteName) {
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	this.CompleteName = CompleteName;
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true);
    }
    private final void onChooseMetric(String Criterion) {
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	ActiveCriterion = Criterion;
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true);
    }
    private final void onChooseSUTLevel(String level) {
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose();
    	ActiveSUTLevel = level;
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
    	graphComposite.setLayout(new GridLayout(2, false));
    	graphComposite.setLayout(new FillLayout());
    	graphComposite.setSize(size);
    	graphComposite.setRedraw(true);
    }
    private final void onChooseTestLevel(String level) {
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	ActiveTestLevel = level;
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true);
    }
    private final void onChooseMouseStyle(String selectedStyle) {
    	this.SelectedMouseStyle = selectedStyle;
    	graphComposite.SetMouseStyle(selectedStyle);
    }
    private final Set<CoverableItem> CreateCoverableItemSet(String Criterion) {
    	if (selectedTestCases.size() == 0) {
//            throw new IllegalArgumentException("testCases.size() == 0");
    		return null;
        }
        if (!checkTestCases(selectedTestCases)) {
            throw new IllegalArgumentException("Not all test cases have the same test session container");
        }
        TestSessionContainer tsc = selectedTestCases.get(0).getTestSession().getTestSessionContainer();
        final Set<CoverableItem> coverableItemSet = new HashSet<CoverableItem>();
        if(Criterion.compareTo("Statement") == 0){
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
        }
        else if(Criterion.compareTo("Branch") == 0){
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
        }
        else if(Criterion.compareTo("Loop") == 0){
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
        }
        else if(Criterion.compareTo("Condition") == 0){
        	tsc.getCode().accept(null, null, null, null,
                    new RootTerm.DefaultVisitor() {
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
        }
        else if(Criterion.compareTo("All") == 0){
        	tsc.getCode().accept(null, null, null, null,
                    new RootTerm.DefaultVisitor() {
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
    class CoverageGraphLink {
    	int id;
    	Integer times;
    	float ratio;
    	String SUTLevel;
    	public CoverageGraphLink(int linkID) {
    		id = linkID;
    		times = 0;
    		ratio = 0;
    	}
    	public String getLable() {
    		if(SUTLevel.compareTo("Item") == 0)
//    			return times.toString();
    			return "";
    		DecimalFormat df = new DecimalFormat("0.0 %");
    		return times.toString() +", "+ df.format(ratio);
    	}
    }
    private edu.uci.ics.jung.graph.Graph<CoverageGraphNode, CoverageGraphLink> createGraph(String Criterion, String SUTLevel, String TestLevel,Boolean ShowOnlyCovered, Boolean CompleteName) {
    	edu.uci.ics.jung.graph.Graph<CoverageGraphNode, CoverageGraphLink> graph = new SparseMultigraph<CoverageGraphNode, CoverageGraphLink>();
    	try{
    		Set<CoverableItem> coverableItemSet = CreateCoverableItemSet(Criterion);
            Vector<CoverageGraphNode> SUTItems = new Vector<CoverageGraphNode>();
            Vector<String> SUTItemsId = new Vector<String>();
            Vector<CoverageGraphNode> TestItems = new Vector<CoverageGraphNode>();
            Vector<String> TestItemsId = new Vector<String>();
            int NumOfSUTNodes = 0;
            int NumOfTestNodes = 0;
            if (selectedTestCases.size() != 0) {
            	CoverageGraphNode SUTNode;
            	CoverageGraphNode TestNode; 
            	List<CoverableItem> SUTItemList = new ArrayList<CoverableItem>(coverableItemSet); 
            	List<CoverableItem> CoveredItemList;

            	//Adding all of the SUT Nodes to the graph:
            	//-----------------------------------------
            	for(int j = 0; j < SUTItemList.size(); j++){
            		boolean ex= false;
            		HierarchyLevel methodLevel = null;
            		try{
            			methodLevel = getSUTItemMethod(SUTItemList.get(j));
            		}
            		catch(IllegalArgumentException e1){
            			// the item is a condition, it is not possible to get the parent statement in the tree (it's parent is null)
            			ex =true;
            		}
            		catch(Exception e2){
            			ex = true;
            		}

            		if(methodLevel != null && !ex){
            			String PackageName = getSUTItemPackage(getSUTItemClass(methodLevel));
            			String ClassName = getSUTItemClass(methodLevel).getName();
            			String MethodName = methodLevel.getName();

            			String ItemName = getSUTItemId(SUTItemList.get(j).getId());
            			String nodeName=getNodeLable(SUTLevel,PackageName,ClassName,MethodName,ItemName);
            			if(!SUTItemsId.contains(nodeName)){
            				SUTNode = new CoverageGraphNode("SUT",
            						SUTLevel,
            						PackageName,
            						ClassName,
            						getSUTItemClass(methodLevel).getLocation().getLocations().get(0),
            						MethodName,
            						methodLevel.getLocation().getLocations().get(0),
            						ItemName,
            						loc,
            						StContent,
            						methodLevel,
            						CompleteName
            				);
            				SUTItemsId.add(SUTNode.getLable());
            				SUTItems.add(SUTNode);
            				NumOfSUTNodes++;
            				if(!ShowOnlyCovered)
            					graph.addVertex(SUTNode);
            			}
            		}
            	}

            	Set<CoverableItem> coveredItemSet;
            	int testsize = 0;
            	if (!selectedTestCases.equals(null))
            		testsize = selectedTestCases.size();
            	for(int i = 0; i < testsize; i++)
            	{

            		//Adding Test Nodes to the graph:
            		//-------------------------------
            		TestCase tc = selectedTestCases.get(i);
            		TestNode = new CoverageGraphNode("Test",
            				TestLevel,
            				getTestNodeName(tc.getName(), "Package"),
            				getTestNodeName(tc.getName(), "Class"),
            				getTestNodeName(tc.getName(), "Method"),
            				CompleteName);
            		String testNodeName = TestNode.getLable();
            		if(!TestItemsId.contains(testNodeName)){
            			TestItemsId.add(TestNode.getLable());
            			TestItems.add(TestNode);
            			graph.addVertex(TestNode);
            			TestNode.Testcases.add(tc);
            			NumOfTestNodes++;
            		}
            		else{
            			for(int k=0;k<NumOfTestNodes; k++)
            				if(TestItems.get(k).getLable().compareTo(testNodeName) == 0)
            					TestNode = TestItems.get(k);
            			TestNode.Testcases.add(tc);
            		}

            		Map<CoverableItem, Long> CoveredItemMap =tc.getCoverageData();
            		coveredItemSet = new HashSet<CoverableItem>();
            		for(int j = 0; j < SUTItemList.size(); j++){
            			coveredItemSet.add(SUTItemList.get(j));
            		}
            		coveredItemSet.retainAll(CoveredItemMap.keySet());
            		CoveredItemList = new ArrayList<CoverableItem>(coveredItemSet); 

            		String nodeName="";
            		for(int j = 0; j < CoveredItemList.size(); j++){
            			boolean ex= false;
            			HierarchyLevel methodLevel = null;
            			HierarchyLevel currentlevel = null;
            			try{
            				methodLevel = getSUTItemMethod(CoveredItemList.get(j));
            			}
            			catch(Exception ec){
            				ex = true;
            			}

            			if(methodLevel != null && !ex){
            				String PackageName = getSUTItemPackage(getSUTItemClass(methodLevel));
            				String ClassName = getSUTItemClass(methodLevel).getName();
            				String MethodName = methodLevel.getName();
            				String ItemName = getSUTItemId(CoveredItemList.get(j).getId());
            				nodeName=getNodeLable(SUTLevel,PackageName,ClassName,MethodName,ItemName);
            				//Adding Edges to the graph:
            				//--------------------------
            				Integer id = graph.getEdgeCount()+1;
            				CoverageGraphLink CoverageLink = graph.findEdge(TestItems.lastElement(),SUTItems.elementAt(SUTItemsId.indexOf(nodeName)));
            				if(CoverageLink == null){   							
            					CoverageLink = new CoverageGraphLink(id);
            					CoverageLink.SUTLevel = SUTLevel;
            				}
            				else
            					graph.removeEdge(CoverageLink);
            				CoverageLink.times ++;

            				CoverageGraphNode CurrentNode = new CoverageGraphNode();
            				for(int k=0;k<NumOfSUTNodes; k++)
            					if(SUTItems.get(k).getLable().compareTo(nodeName) == 0)
            						CurrentNode = SUTItems.get(k);
            				if(CurrentNode != null){
            					if(ShowOnlyCovered)
            						graph.addVertex(CurrentNode);

            					//For calculating Ratio:
            					if(SUTLevel.compareTo("Method")==0)
            						currentlevel = methodLevel;
            					else if(SUTLevel.compareTo("Class")==0)
            						currentlevel = topLevel.getParent(methodLevel);
            					else if(SUTLevel.compareTo("Package")==0){
            						currentlevel = topLevel.getParent(methodLevel);
            						currentlevel = topLevel.getParent(currentlevel);
            					}
            					if(currentlevel != null){
            						CoverageResult coverageResult = null;
            						if(Criterion.compareTo("Statement") == 0){
            							StatementCoverage coverageMetric = StatementCoverage.getInstance();
            							coverageResult = coverageMetric.getCoverage(TestNode.Testcases, currentlevel);
            						}
            						else if(Criterion.compareTo("Branch") == 0){
            							BranchCoverage coverageMetric = BranchCoverage.getInstance();
            							coverageResult = coverageMetric.getCoverage(TestNode.Testcases, currentlevel);
            						}
            						else if(Criterion.compareTo("Loop") == 0){
            							LoopCoverage coverageMetric = LoopCoverage.getInstance();
            							coverageResult = coverageMetric.getCoverage(TestNode.Testcases, currentlevel);
            						}
            						else if(Criterion.compareTo("Term") == 0){
            							TermCoverage coverageMetric = TermCoverage.getInstance();
            							coverageResult = coverageMetric.getCoverage(TestNode.Testcases, currentlevel);
            						}
            						if(coverageResult != null){
            							float coverage = 0f;
            							if (coverageResult.getTotalItems() > 0) {
            								coverage = ((float) coverageResult.getCoveredItems() / coverageResult.getTotalItems());
            							}
            							CoverageLink.ratio = coverage;
            						}
            					}
            					else
            						CoverageLink.ratio = 2;
            					graph.addEdge(CoverageLink,TestItems.lastElement(),CurrentNode,EdgeType.DIRECTED);
            				}
            			}
            		}
            		coveredItemSet.clear();
            	}
            }
    	}
    	catch(Exception ex){
    		return null;
    	}
    	return graph;
    }  
    private Runnable getTSCChangedRunnable(final ActiveTSContainerInfo tscInfo) {
        return new Runnable() {
            @Override
			public void run() {
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
                if (tscInfo != null) {
                    onActiveTestCasesChanged(tscInfo.getActiveTestCases());
                } else {
                    onActiveTestCasesChanged(new HashSet<TestCase>());
                }
            }
        };
    }
    private final void refreshActiveTestCaseList(Set<TestCase> activetestCases) {
        this.selectedTestCases.clear();
        this.selectedTestCases.addAll(sortTestCases(activetestCases));
    }
    private final void onTSCChanged(Set<TestCase> activetestCases) {
    	synchronized (lock) {
            this.calculationPending = false;
        }      
    	try{
    	refreshActiveTestCaseList(activetestCases);
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true);
    	}
    	catch(Exception e){
//    		try{
//    		File LogFile = new File("c:\\Log2.txt");
//	    	PrintWriter pw = new PrintWriter(new FileWriter(LogFile));
//	    	pw.println(e.toString());
//	    	pw.println(topLevel.toString());
//	    	pw.close();
//    		}
//    		catch(Exception e2){}
    	}
    }
    private final void onActiveTestCasesChanged(Set<TestCase> activetestCases) {
    	refreshActiveTestCaseList(activetestCases);
    	org.eclipse.swt.graphics.Point size = graphComposite.getSize();
    	graphComposite.dispose(); 
    	graphComposite = new GraphComposite(this.sashForm, SWT.NONE,size,createGraph(ActiveCriterion,ActiveSUTLevel,ActiveTestLevel,SelectedOnlyCovered,CompleteName),SelectedMouseStyle);
        graphComposite.setLayout(new GridLayout(2, false));
        graphComposite.setLayout(new FillLayout());
        graphComposite.setSize(size);
        graphComposite.setRedraw(true); 	
    }
    private final class GraphComposite extends Composite {

    	
    	VisualizationComposite<CoverageGraphNode, CoverageGraphLink> vv;

    	final DefaultModalGraphMouse graphMouse = new DefaultModalGraphMouse();
    	
    	Layout<CoverageGraphNode, CoverageGraphLink> layout;

    	public GraphComposite(Composite parent, int style, org.eclipse.swt.graphics.Point size, edu.uci.ics.jung.graph.Graph<CoverageGraphNode, CoverageGraphLink> graph,String MouseStyle) {
    		super(parent, style);
    		setLayout(new GridLayout());

    		if(graph != null){
    			layout = new CoverageGraphLayout<CoverageGraphNode, CoverageGraphLink>(graph,size);
//    			layout.setSize(new Dimension(size.x,size.y));

    			final GraphZoomScrollPane<CoverageGraphNode, CoverageGraphLink> panel = new GraphZoomScrollPane<CoverageGraphNode, CoverageGraphLink>(this, SWT.NONE, layout, new Dimension(size.x,size.y));
    			GridData gridData = new GridData();
    			gridData.grabExcessHorizontalSpace = true;
    			gridData.grabExcessVerticalSpace = true;
    			gridData.horizontalAlignment = GridData.FILL;
    			gridData.verticalAlignment = GridData.FILL;
    			panel.setLayoutData(gridData);

    			vv =  panel.vv;
    			vv.setBackground(Color.white);
    			
    			//Setting Labels for each node:
    			Transformer<CoverageGraphNode,String> lableTransformer = new Transformer<CoverageGraphNode,String>() {
    				@Override
					public String transform(CoverageGraphNode node) {
    					if(node.CompletName)
    						return node.getLable();
    					else
    						return node.getShortLable();
    				}
    			};
    			vv.getRenderContext().setVertexLabelTransformer(lableTransformer);		
    			
    			//Changing the Shape of each node:
    			final Rectangle rectangle = new Rectangle();
    			Transformer<CoverageGraphNode,Shape> vertexTransformer = new Transformer<CoverageGraphNode,Shape>() {
    				@Override
					public Shape transform(CoverageGraphNode node) {
    					int length;
    					if(node.CompletName)
    						length = node.getLable().length()*8;
    					else
    						length = node.getShortLable().length()*8;
    					rectangle.setSize(length, 16);
    					if(node.type == "SUT"){
    						rectangle.setLocation(0,-8);
    						return rectangle;
    					}
    					else{
    						rectangle.setLocation(-length,-8);				
    						return rectangle;
    					}
    				}
    			};
    			vv.getRenderContext().setVertexShapeTransformer(vertexTransformer);
    			
    			//Changing the Color of each node:
    			Transformer<CoverageGraphNode,Paint> vertexPaint = new Transformer<CoverageGraphNode,Paint>() {
    				@Override
					public Paint transform(CoverageGraphNode node) {
    					if(node.type == "SUT")
    						return Color.orange;
    					else
    						return Color.green;
    				}
    			};	
    			vv.getRenderContext().setVertexFillPaintTransformer(vertexPaint);
    			
    			//Changing the font of each node:
    			Transformer<CoverageGraphNode,Font> vertexFont = new Transformer<CoverageGraphNode,Font>() {
    				@Override
					public Font transform(CoverageGraphNode node) {
    					return new Font("Monospaced",Font.BOLD,15);
    				}
    			};	
    			vv.getRenderContext().setVertexFontTransformer(vertexFont);
    			
    			//Setting Labels for each edge:
    			Transformer<CoverageGraphLink,String> edgeLableTransformer = new Transformer<CoverageGraphLink,String>() {
    				@Override
					public String transform(CoverageGraphLink edge) {
    					return edge.getLable();
    				}
    			};
    			vv.getRenderContext().setEdgeLabelTransformer(edgeLableTransformer);
    			vv.getRenderContext().setEdgeShapeTransformer(new EdgeShape.Line<CoverageGraphNode,CoverageGraphLink>());
    			
    			vv.getRenderer().getVertexLabelRenderer().setPosition(Position.CNTR);
 
    			GridData gd = new GridData();
    			gd.grabExcessHorizontalSpace = true;
    			gd.grabExcessVerticalSpace = true;
    			gd.horizontalAlignment = GridData.FILL;
    			gd.verticalAlignment = GridData.FILL;
    			vv.getComposite().setLayoutData(gd);
    			

				vv.setGraphMouse(graphMouse);
				vv.addKeyListener(graphMouse.getModeKeyListener());

				final ScalingControl scaler = new CrossoverScalingControl();
				vv.scaleToLayout(scaler);

				if (MouseStyle.compareTo("Picking")==0)
					graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING);
				else
					graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.TRANSFORMING);
    		}
    		class JumperToEditor implements MouseListener<java.awt.event.MouseEvent>{

				@Override
				public void mouseClicked(
						MouseEvent<java.awt.event.MouseEvent> arg0) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void mouseDoubleClicked(
						MouseEvent<java.awt.event.MouseEvent> mouseEvent) {
					// TODO Auto-generated method stub
					java.awt.Point p = mouseEvent.getPoint();
					GraphElementAccessor<CoverageGraphNode, CoverageGraphLink> ps = vv.getServer().getPickSupport();
					if(ps != null){
						CoverageGraphNode SelectedNode = ps.getVertex(layout, p.getX(), p.getY());

						ITextEditor editor = EclipseMASTLinkage.openClassInEditor(SelectedNode.HL,tsc);

				        if (editor == null) {
				            /* failed: no suitable Resource can be opened */
				        } else {
				            /* show hLev in the Editor */
				        	if(SelectedNode.level == "Method")
				        		EclipseMASTLinkage.showInEditor(editor,SelectedNode.MethodLocation);
				        	else if (SelectedNode.level == "Class")
				        		EclipseMASTLinkage.showInEditor(editor,SelectedNode.ClassLocation);
				        	else if (SelectedNode.level == "Item")
				        		EclipseMASTLinkage.showInEditor(editor,SelectedNode.itemLocation);
				        }
					}
				}

				@Override
				public void mouseEntered(
						MouseEvent<java.awt.event.MouseEvent> mouseEvent) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void mouseExited(
						MouseEvent<java.awt.event.MouseEvent> arg0) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void mousePressed(
						MouseEvent<java.awt.event.MouseEvent> arg0) {
					// TODO Auto-generated method stub
					
				}

				@Override
				public void mouseReleased(
						MouseEvent<java.awt.event.MouseEvent> arg0) {
					// TODO Auto-generated method stub
					
				}
    			
    		};
    		JumperToEditor jte = new JumperToEditor();
			ScreenDevice<Composite> SD = vv.getScreenDevice();
			SD.addMouseListener(jte);
    	}
    	public void SetMouseStyle(String Style){
    		if (Style.compareTo("Picking")==0)
				graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.PICKING);
			else
				graphMouse.setMode(edu.uci.ics.jung.visualization.control.ModalGraphMouse.Mode.TRANSFORMING);
    	}

    	/**
    	 * 
    	 */
  
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
                            if (CoverageGraphView.this.calculationPending) {
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
            	if (CoverageGraphView.this.calculationPending) {
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
            	if (CoverageGraphView.this.calculationPending) {
                    return;
                }
            	CoverageGraphView.this.calculationPending = true;

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
            	if (CoverageGraphView.this.calculationPending) {
                    return;
                }
                CoverageGraphView.this.calculationPending = true;

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
            	if (CoverageGraphView.this.calculationPending) {
                    return;
                }
                CoverageGraphView.this.calculationPending = true;

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
    		org.eclipse.swt.graphics.Point size = graphComposite.getSize();
        	graphComposite.dispose(); 
//        	graphComposite = new Composite(sashForm, SWT.NONE);
            graphComposite.setLayout(new GridLayout(2, false));
            graphComposite.setLayout(new FillLayout());
//            graphComposite.setSize(size);
//            graphComposite.setRedraw(true);
//        	Graph g = CreateGraph(graphComposite,ActiveCriterion,ActiveSUTLevel,ActiveTestLevel);		
//    		g.setLayoutAlgorithm(new TreeLayoutAlgorithm(LayoutStyles.ENFORCE_BOUNDS), true);
//    		graphComposite.layout();
    	}
    }
    private String getTestNodeName(String fullname, String level){
    	int pckSeperator = fullname.lastIndexOf('.');
    	int classSeperator = fullname.indexOf(':');
    	if(level.compareTo("Package") == 0){
    		if (pckSeperator == -1)
    			return "default package";
    		else
    			return fullname.substring(0, pckSeperator);
    	}
    	else if(level.compareTo("Class") == 0){
    		return fullname.substring(pckSeperator+1,classSeperator);
    	}
    	else if(level.compareTo("Method") == 0){
    		return fullname.substring(classSeperator+1,fullname.length());
    	}
    	return null;
    }
    
    private String getSUTNodeName(String fullprefix,String id, String level){
    	int pckSeperator = fullprefix.substring(0,fullprefix.lastIndexOf('.')).lastIndexOf('.');
    	int classSeperator = fullprefix.lastIndexOf('.');
    	if(level.compareTo("Package") == 0){
    		if (pckSeperator == -1)
    			return "default package";
    		else
    			return fullprefix.substring(0, pckSeperator);
    	}
    	else if(level.compareTo("Class") == 0){
    		return fullprefix.substring(pckSeperator+1, classSeperator);
    	}
    	else if(level.compareTo("Method") == 0){ // we don't have information for this level 
    		return "";
    	}
    	else if(level.compareTo("Item") == 0){
    		return id;
    	}
    	return null;
    }
    private String getSUTItemPackage(HierarchyLevel level){
    	String Package ="";
    	try{
    		Package = topLevel.getParent(level).getName();
    		level = topLevel.getParent(level);
    		level = topLevel.getParent(level);
    		while(level != topLevel){
    			Package = level.getName() +"."+ Package;
    			level = topLevel.getParent(level);
    		}
    	}
    	catch (Exception ex){
    		return "default package";
    	}
    	return Package;
    }
    private HierarchyLevel getSUTItemClass(HierarchyLevel methodLevel){
    	return topLevel.getParent(methodLevel);
    }
    private HierarchyLevel getSUTItemMethod(CoverableItem item){
    	String FileName1 = "";
		String content1 ="";
		if (getSUTItemStatement(item) instanceof Branch){
			FileName1 = ((Branch)getSUTItemStatement(item)).getLocation().getLocations().get(0).getFile().getFileName();
			content1 = ((Branch)getSUTItemStatement(item)).getLocation().getLocations().get(0).getContent();
			loc = ((Branch)getSUTItemStatement(item)).getLocation().getLocations().get(0);
    		StContent = ((Branch)getSUTItemStatement(item)).getLocation().getLocations().get(0).getContent(); 		
		}
		else{
			FileName1 = ((Statement)getSUTItemStatement(item)).getLocation().getLocations().get(0).getFile().getFileName();
    		content1 = ((Statement)getSUTItemStatement(item)).getLocation().getLocations().get(0).getContent();
    		loc = ((Statement)getSUTItemStatement(item)).getLocation().getLocations().get(0);
    		StContent = ((Statement)getSUTItemStatement(item)).getLocation().getLocations().get(0).getContent();
    		
		}
		
    	List<HierarchyLevel> allMethods = oldestChildrenOfType(Type.METHOD,topLevel);
    	for(int i=0;i<allMethods.size();i++)
    		for (StatementSequence statements : allMethods.get(i).getSequences()) 
            	for (Statement statement : statements.getStatements()) {	
            		String FileName2 = statement.getLocation().getLocations().get(0).getFile().getFileName();
            		String content2 = statement.getLocation().getLocations().get(0).getContent();
            		if(FileName1.compareTo(FileName2)==0 && content2.contains(content1)){
            			st = statement;
            			return allMethods.get(i);  
            		}
            	}
    	st = null;		
    	return null;
    }
    private Object getSUTItemStatement(CoverableItem item){
    	return topLevel.getParent(item);
    	
    }
    private String getSUTItemId(String Id){
    	String ItemId = "";
    	String idCode = "";
    	if(Id.charAt(0)=='L'){
    		idCode = Id.substring(1,Id.lastIndexOf('-'));
    		for(int i=idCode.length();i<5;i++){
    			idCode = "0"+idCode;
    		}
    		if(Id.lastIndexOf('-') < Id.length())
    			ItemId = "L"+idCode+Id.substring(Id.lastIndexOf('-'));
    		else
    			ItemId = "L"+idCode;
    	}
    	else{
    		idCode = Id.substring(1);
    		for(int i=idCode.length();i<5;i++){
    			idCode = "0"+idCode;
    		}
    		ItemId = Id.substring(0,1) + idCode;
    	}
    	return ItemId;
    }
    private String getNodeLable(String level,String PackageName,String ClassName,String MethodName,String ItemName){
    		if(level.compareTo("Package") == 0){      		
        		return PackageName;
        	}
        	else if(level.compareTo("Class") == 0){
        		return PackageName +"."+ ClassName;
        	}
        	else if(level.compareTo("Method") == 0){ 
        		return PackageName +"."+ ClassName +"."+ MethodName;
        	}
        	else if(level.compareTo("Item") == 0){
        		return PackageName +"."+ ClassName +"."+ MethodName +"."+ ItemName;
        	}
        	return "";
    		
    }
    private final List<IAction> createCorrelationActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> correlationActions = new Vector<IAction>();

      //Create Action For Statement Criterion:
            String name = "Statement Coverage";
            IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseMetric("Statement");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(true);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    true));
            correlationActions.add(action);
            
       //Create Action For Branch Criterion:
            name = "Branch Coverage";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseMetric("Branch");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            correlationActions.add(action);

       //Create Action For Condition Criterion:
//            name = "Condition Coverage";
//            action = new Action(name, IAction.AS_RADIO_BUTTON) {
//                @Override
//                public void run() {
//                    onChooseMetric("Condition");
//                }
//            };
//            action.setImageDescriptor(platformImages
//                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));
//
//            action.setChecked(false);
////            (getBooleanFromMemento(escapeWhiteSpaces(name),
////                    false));
//            correlationActions.add(action);    

       //Create Action For Loop Criterion:
//            name = "Loop Coverage";
//            action = new Action(name, IAction.AS_RADIO_BUTTON) {
//                @Override
//                public void run() {
//                    onChooseMetric("Loop");
//                }
//            };
//            action.setImageDescriptor(platformImages
//                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));
//
//            action.setChecked(false);
////            (getBooleanFromMemento(escapeWhiteSpaces(name),
////                    false));
//            correlationActions.add(action);
            
       //Create Action For All Criteria:
            name = "All Metrics";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseMetric("All");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            correlationActions.add(action);
           
        return correlationActions;
    }

    private final List<IAction> createSUTLevelsActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> SUTLevelActions = new Vector<IAction>();

        //Create Action For Package Level:
            String name = "SUT Package";
            IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseSUTLevel("Package");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            SUTLevelActions.add(action);
        
         //Create Action For Class Level:
            name = "SUT Class";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseSUTLevel("Class");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            SUTLevelActions.add(action); 
        //Create Action For Method Level:
            name = "SUT Method";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseSUTLevel("Method");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(true);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    true));
            SUTLevelActions.add(action); 
            
        //Create Action For Item Level:
            name = "SUT Item";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseSUTLevel("Item");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            SUTLevelActions.add(action);   

        return SUTLevelActions;
    }
    
    private final List<IAction> createTestLevelsActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> TestLevelActions = new Vector<IAction>();

        //Create Action For Package Level:
            String name = "Test Package";
            IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseTestLevel("Package");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            TestLevelActions.add(action);
            
        //Create Action For Class Level:
            name = "Test Class";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseTestLevel("Class");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    false));
            TestLevelActions.add(action); 
            
        //Create Action For Method Level:
            name = "Test Method";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseTestLevel("Method");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(true);
//            (getBooleanFromMemento(escapeWhiteSpaces(name),
//                    true));
            TestLevelActions.add(action);   

        return TestLevelActions;
    }
    private final List<IAction> createMouseStyleActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> MouseStyleActions = new Vector<IAction>();

        //Create Action For Picking:
            String name = "Picking";
            IAction action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                	onChooseMouseStyle("Picking");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(true);
            MouseStyleActions.add(action);
            
        //Create Action For Transforming:
            name = "Moving";
            action = new Action(name, IAction.AS_RADIO_BUTTON) {
                @Override
                public void run() {
                    onChooseMouseStyle("Moving");
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
            MouseStyleActions.add(action); 
        return MouseStyleActions;
    }
    private final List<IAction> createOptionsActions() {
        IWorkbench workbench = PlatformUI.getWorkbench();
        ISharedImages platformImages = workbench.getSharedImages();

        final List<IAction> MouseStyleActions = new Vector<IAction>();

        //Create Action For Picking:
            String name = "Show Only Covered Items";
            IAction action = new Action(name, IAction.AS_CHECK_BOX) {
                @Override
                public void run() {
                	onChooseShowAll(isChecked());
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

            action.setChecked(false);
            MouseStyleActions.add(action);
            
          //Create Action For Picking:
            name = "Show Complete Names";
            action = new Action(name, IAction.AS_CHECK_BOX) {
                @Override
                public void run() {
                	onChooseCompleteName(isChecked());
                }
            };
            action.setImageDescriptor(platformImages
                    .getImageDescriptor(ISharedImages.IMG_DEF_VIEW));

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
    }

    private final String escapeWhiteSpaces(String text) {
        return text.replace(' ', '_');
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

    private final class MetricMenuCreator implements IMenuCreator {
        @Override
		public void dispose() {
            // Nothing to do here.
        }

        @Override
		public Menu getMenu(Control parent) {
            Menu dropDownMenu = new Menu(parent);
            for (IAction action : CoverageGraphView.this.correlationActions) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

                contributionItem.fill(dropDownMenu, -1);

            }
            return dropDownMenu;
        }

        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }
    }
    private final class SUTLevelMenuCreator implements IMenuCreator {
        @Override
		public void dispose() {
            // Nothing to do here.
        }
        
        @Override
		public Menu getMenu(Control parent) {
            Menu dropDownMenu = new Menu(parent);
            for (IAction action : CoverageGraphView.this.SUTLevels) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

                contributionItem.fill(dropDownMenu, -1);

            }
            return dropDownMenu;
        }
        
        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }
    }
    private final class MouseStyleMenuCreator implements IMenuCreator {
        @Override
		public void dispose() {
            // Nothing to do here.
        }
        
        @Override
		public Menu getMenu(Control parent) {
            Menu dropDownMenu = new Menu(parent);
            for (IAction action : CoverageGraphView.this.MouseStyle) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

                contributionItem.fill(dropDownMenu, -1);

            }
            return dropDownMenu;
        }
        
        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }
    }
    private final class OptionMenuCreator implements IMenuCreator {
        @Override
		public void dispose() {
            // Nothing to do here.
        }
        
        @Override
		public Menu getMenu(Control parent) {
            Menu dropDownMenu = new Menu(parent);
            for (IAction action : CoverageGraphView.this.Options) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

                contributionItem.fill(dropDownMenu, -1);

            }
            return dropDownMenu;
        }
        
        @Override
		public Menu getMenu(Menu parent) {
            return null;
        }
    }
    private final class TestLevelMenuCreator implements IMenuCreator {

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
            for (IAction action : CoverageGraphView.this.TestLevels) {

                ActionContributionItem contributionItem = new ActionContributionItem(
                        action);

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
}