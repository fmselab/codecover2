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

import java.text.DecimalFormat;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManagerListener;
import org.codecover.metrics.coverage.CoverageResult;
import org.codecover.metrics.coverage.TermCoverage;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.mast.BasicBooleanTerm;
import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanAssignmentMap;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.OperatorTerm;
import org.codecover.model.mast.RootTerm;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.criteria.Criterion;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.part.ViewPart;

/**
 * This {@link BooleanAnalyzer} is a view in eclipse. It provides the user with
 * the opportunity to view the conditions contained in their SUTs and the
 * assignments, that occurred during the execution. Also displayed is the amount
 * of {@link TermCoverage} the condition achieved and which of the
 * {@link BasicBooleanTerm}s are covered under this {@link Criterion}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: BooleanAnalyzer.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class BooleanAnalyzer extends ViewPart {

    private static final String LABEL_CLASS = Messages
            .getString("BooleanAnalyzer.LABEL_CLASS"); //$NON-NLS-1$

    private static final String LABEL_CONDITION = Messages
            .getString("BooleanAnalyzer.LABEL_CONDITION"); //$NON-NLS-1$

    private static final String HEADER_TEST_CASES = Messages
            .getString("BooleanAnalyzer.HEADER_TEST_CASES"); //$NON-NLS-1$

    private static final String HEADER_RESULT = Messages
            .getString("BooleanAnalyzer.HEADER_RESULT"); //$NON-NLS-1$

    private static final String STATUS_BAR_TEXT = Messages
            .getString("BooleanAnalyzer.STATUS_BAR_TEXT"); //$NON-NLS-1$

    private static final String COLUMN_STORE_KEY = "columnStoreKeyBooleanTerm"; //$NON-NLS-1$

    private TableViewer tableViewer;

    private Label statusBar;

    private Combo classCombo;

    private Combo conditionCombo;

    private final List<HierarchyLevel> classList;

    private final List<RootTerm> conditionList;

    private final TSManagerListener managerListener;

    /**
     * Constructor
     */
    public BooleanAnalyzer() {
        this.classList = new LinkedList<HierarchyLevel>();
        this.conditionList = new LinkedList<RootTerm>();
        this.managerListener = new TSManagerListener();

        CodeCoverPlugin.getDefault().getTSContainerManager().addListener(
                this.managerListener);
        CodeCoverPlugin.getDefault().setBooleanAnalyzer(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
     */
    @Override
    public void createPartControl(Composite parent) {
        parent.setLayout(new GridLayout(4, false));

        Label classLabel = new Label(parent, SWT.NONE);
        classLabel.setText(LABEL_CLASS);

        this.classCombo = new Combo(parent, SWT.DROP_DOWN | SWT.READ_ONLY);
        this.classCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin
                        .getDefault().getTSContainerManager()
                        .getActiveTSContainer();

                fillConditionList(getSelectedClass());
                fillConditionComboBox();

                createTableColumns(getSelectedRootTerm());

                if (activeTSCInfo != null) {
                    fillTable(new LinkedList<TestCase>(activeTSCInfo
                            .getActiveTestCases()), getSelectedRootTerm());
                } else {
                    fillTable(new LinkedList<TestCase>(), getSelectedRootTerm());
                }
            }
        });

        Label conditionLabel = new Label(parent, SWT.NONE);
        conditionLabel.setText(LABEL_CONDITION);

        this.conditionCombo = new Combo(parent, SWT.DROP_DOWN | SWT.READ_ONLY);
        this.conditionCombo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin
                        .getDefault().getTSContainerManager()
                        .getActiveTSContainer();

                createTableColumns(getSelectedRootTerm());

                if (activeTSCInfo != null) {
                    fillTable(new LinkedList<TestCase>(activeTSCInfo
                            .getActiveTestCases()), getSelectedRootTerm());
                } else {
                    fillTable(new LinkedList<TestCase>(), getSelectedRootTerm());
                }
            }
        });
        this.conditionCombo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER,
                true, false));

        this.tableViewer = new TableViewer(parent, SWT.HIDE_SELECTION
                | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER | SWT.FULL_SELECTION
                | SWT.MULTI);
        final GridData gridData = new GridData(SWT.FILL, SWT.FILL, true, true);
        gridData.horizontalSpan = 4;
        this.tableViewer.getTable().setLayoutData(gridData);
        this.tableViewer.getTable().setHeaderVisible(true);
        this.tableViewer.setLabelProvider(new TableLabelAndColorProvider());
        this.tableViewer.setContentProvider(new TableContentProvider());

        this.statusBar = new Label(parent, SWT.NONE);
        final GridData statusLabelGridData = new GridData(SWT.FILL, SWT.CENTER,
                true, false);
        statusLabelGridData.horizontalSpan = 4;
        this.statusBar.setLayoutData(statusLabelGridData);

        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        TestSessionContainer currentTSC;
        if (activeTSCInfo != null) {
            currentTSC = activeTSCInfo.getTestSessionContainer();
        } else {
            currentTSC = null;
        }

        fillClassList(currentTSC);

        fillClassComboBox();

        fillConditionList(getSelectedClass());

        fillConditionComboBox();

        createTableColumns(getSelectedRootTerm());

        if (activeTSCInfo != null) {
            fillTable(new LinkedList<TestCase>(activeTSCInfo
                    .getActiveTestCases()), getSelectedRootTerm());
        } else {
            fillTable(new LinkedList<TestCase>(), getSelectedRootTerm());
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#dispose()
     */
    @Override
    public void dispose() {
        CodeCoverPlugin.getDefault().getTSContainerManager().removeListener(
                this.managerListener);
    }

    private final RootTerm getSelectedRootTerm() {
        int index = this.conditionCombo.getSelectionIndex();

        if (index == -1) {
            return null;
        }

        if (index >= this.conditionList.size()) {
            CodeCoverPlugin
                    .getDefault()
                    .getLogger()
                    .error(
                            "Boolean Analyzer: Invalid index in the list of conditions"); //$NON-NLS-1$
            return null;
        }

        return this.conditionList.get(index);
    }

    private final HierarchyLevel getSelectedClass() {
        int index = this.classCombo.getSelectionIndex();

        if (index == -1) {
            return null;
        }

        if (index >= this.classList.size()) {
            CodeCoverPlugin.getDefault().getLogger().error(
                    "Boolean Analyzer: Invalid index in the list of classes"); //$NON-NLS-1$
            return null;
        }

        return this.classList.get(index);
    }

    private final void fillConditionComboBox() {
        this.conditionCombo.removeAll();

        String[] itemNames = new String[this.conditionList.size()];

        for (int i = 0; i < itemNames.length; i++) {
            itemNames[i] = getConditionNameFromRootTerm(this.conditionList
                    .get(i));
        }

        if (this.conditionList.size() > 0) {
            this.conditionCombo.setItems(itemNames);
        }

        this.conditionCombo.select(0);
    }

    /**
     * Gets the full text of a condition for display in the ComboBox.
     * 
     * @param rootTerm
     *            the {@link RootTerm} to be displayed.
     * @return the string representing the {@link RootTerm}.
     */
    private final String getConditionNameFromRootTerm(RootTerm rootTerm) {
        final StringBuilder sb = new StringBuilder();

        traverseRootTerm(rootTerm.getTerm(), new Visitor() {
            @Override
			public void visit(BasicBooleanTerm booleanTerm) {
                for (Location location : booleanTerm.getLocation()
                        .getLocations()) {
                    appendString(sb, location.getContent());
                }
            }

            @Override
			public void visit(OperatorTerm operatorTerm) {
                appendString(sb, operatorTerm.getShortNameOfOperator());
            }

            @Override
			public void visit(OperatorTerm operatorTerm, String name) {
                appendString(sb, name);
            }

            @Override
			public void visit(String name) {
                appendString(sb, name);
            }

            private void appendString(final StringBuilder sb, String name) {
                sb.append(name);
                sb.append(" "); //$NON-NLS-1$
            }

        });

        String name = sb.toString();
        // Compact the name of the condition, by removing unnecessary spaces
        // between braces.
        name = name.replaceAll("\\(\\s", "("); //$NON-NLS-1$ //$NON-NLS-2$
        name = name.replaceAll("\\s\\)", ")"); //$NON-NLS-1$ //$NON-NLS-2$
        return name;
    }

    private final void fillConditionList(HierarchyLevel classLevel) {
        this.conditionList.clear();

        if (classLevel == null) {
            return;
        }

        classLevel.accept(null, null, null, null, new RootTerm.Visitor() {
            @Override
			public void visit(RootTerm term) {
                BooleanAnalyzer.this.conditionList.add(term);
            }
        }, null, null, null, null);
    }

    private final void fillClassComboBox() {
        this.classCombo.removeAll();

        String[] itemNames = new String[this.classList.size()];

        for (int i = 0; i < itemNames.length; i++) {
            itemNames[i] = this.classList.get(i).getName();
        }

        if (this.classList.size() > 0) {
            this.classCombo.setItems(itemNames);
        }

        this.classCombo.select(0);
    }

    private final void fillClassList(TestSessionContainer testSessionContainer) {
        this.classList.clear();

        if (testSessionContainer == null) {
            return;
        }

        HierarchyLevel topLevel = testSessionContainer.getCode();

        topLevel.accept(new HierarchyLevel.Visitor() {
            @Override
			public void visit(HierarchyLevel hierarchyLevel) {
                if (isHierarchyLevelTypeForClass(hierarchyLevel.getType()
                        .getInternalName())) {
                    BooleanAnalyzer.this.classList.add(hierarchyLevel);
                }
            }
        }, null, null, null, null, null, null, null, null);
    }

    private final boolean isHierarchyLevelTypeForClass(String internalName) {
        if (internalName.equals("class") || internalName.equals("interface") //$NON-NLS-1$ //$NON-NLS-2$
                || internalName.equals("enum") //$NON-NLS-1$
                || internalName.equals("@interface")) { //$NON-NLS-1$
            return true;
        }
        return false;
    }

    /**
     * Creates the {@link TableColumn} according to the {@link OperatorTerm}s
     * and {@link BasicBooleanTerm}s of the given {@link RootTerm}.
     * 
     * @param rootTerm
     *            the given {@link RootTerm}, to be used in the creation of the
     *            columns.
     */
    private final void createTableColumns(RootTerm rootTerm) {
        // Remove all the old table columns
        for (TableColumn tableColumn : this.tableViewer.getTable().getColumns()) {
            tableColumn.dispose();
        }

        if (rootTerm == null) {
            return;
        }

        // We traverse the RootTerm and create TableColums for every time the
        // visitor is called. The BooleanTerm is stored in the TableColum for
        // later usage and ease of the retrieval of the information, that is to
        // be displayed in the table.
        traverseRootTerm(rootTerm.getTerm(), new Visitor() {
            @Override
			public void visit(BasicBooleanTerm booleanTerm) {
                StringBuilder sb = new StringBuilder();
                for (Location location : booleanTerm.getLocation()
                        .getLocations()) {
                    sb.append(location.getContent());
                    sb.append(" "); //$NON-NLS-1$
                }
                createTableColumn(sb.toString(), COLUMN_STORE_KEY, booleanTerm);
            }

            @Override
			public void visit(OperatorTerm operatorTerm) {
                createTableColumn(operatorTerm.getShortNameOfOperator(),
                        COLUMN_STORE_KEY, operatorTerm);
            }

            @Override
			public void visit(OperatorTerm operatorTerm, String name) {
                createTableColumn(name, COLUMN_STORE_KEY, operatorTerm);
            }

            @Override
			public void visit(String name) {
                createTableColumn(name);
            }

            /**
             * Creates a {@link TableColumn} with the given text and returns it.
             * 
             * @param text
             *            the header text.
             * @return the created {@link TableColumn}
             */
            private TableColumn createTableColumn(String text) {
                TableColumn tableColumn = new TableColumn(
                        BooleanAnalyzer.this.tableViewer.getTable(), SWT.CENTER);

                // The columns seem to consume a leading '&' so we add another
                // one, which seems to work. Is an ugly workaround.
                tableColumn.setText(escapeAmpersand(text));
                tableColumn.pack();

                return tableColumn;
            }

            private String escapeAmpersand(String input) {
                StringBuilder sb = new StringBuilder();

                for (int i = 0; i < input.length(); i++) {
                    char c = input.charAt(i);
                    sb.append(c);
                    if (c == '&') {
                        sb.append(c);
                    }
                }

                return sb.toString();
            }

            /**
             * Creates a {@link TableColumn} with the given text and stores the
             * given data under the given key and returns the created
             * {@link TableColumn}.
             * 
             * @param text
             *            the header text.
             * @param key
             *            the key to be used in the storage
             * @param data
             *            the data to be stored.
             * @return the created {@link TableColumn}
             */
            private TableColumn createTableColumn(String text, String key,
                    Object data) {
                TableColumn tableColumn = createTableColumn(text);
                tableColumn.setData(key, data);

                return tableColumn;
            }
        });

        TableColumn resultColumn = new TableColumn(this.tableViewer.getTable(),
                SWT.CENTER);
        resultColumn.setText(HEADER_RESULT);
        resultColumn.pack();

        TableColumn testCaseColumn = new TableColumn(this.tableViewer
                .getTable(), SWT.LEFT);
        testCaseColumn.setText(HEADER_TEST_CASES);
        testCaseColumn.pack();
    }

 
    private final void traverseRootTerm(BooleanTerm booleanTerm, Visitor visitor) {
        if (booleanTerm instanceof BasicBooleanTerm) {
            visitor.visit((BasicBooleanTerm) booleanTerm);
        } else if (booleanTerm instanceof OperatorTerm) {
            OperatorTerm operatorTerm = (OperatorTerm) booleanTerm;
            switch (operatorTerm.getOperator().getArity()) {
                case 0:
                    visitor.visit(operatorTerm);
                    break;
                case 1:
                    visitor.visit(operatorTerm);
                    traverseRootTerm(operatorTerm.getOperands().get(0), visitor);
                    break;
                case 2:
                    visitor.visit("("); //$NON-NLS-1$
                    traverseRootTerm(operatorTerm.getOperands().get(0), visitor);
                    visitor.visit(operatorTerm);
                    traverseRootTerm(operatorTerm.getOperands().get(1), visitor);
                    visitor.visit(")"); //$NON-NLS-1$
                    break;
                case 3:
                    visitor.visit("("); //$NON-NLS-1$
                    traverseRootTerm(operatorTerm.getOperands().get(0), visitor);

                    String firstPart;
                    firstPart = operatorTerm.getLocation().getLocations()
                            .get(0).getContent();

                    visitor.visit(operatorTerm, firstPart);
                    traverseRootTerm(operatorTerm.getOperands().get(1), visitor);

                    String secondPart;
                    secondPart = operatorTerm.getLocation().getLocations().get(
                            1).getContent();

                    visitor.visit(secondPart);
                    traverseRootTerm(operatorTerm.getOperands().get(2), visitor);
                    visitor.visit(")"); //$NON-NLS-1$
                    break;
                default:
                    /*
                     * XXX for arity 4 and above we don't reorder the condition.
                     */
                    visitor.visit("("); //$NON-NLS-1$
                    visitor.visit(operatorTerm);
                    for (BooleanTerm term : operatorTerm.getOperands()) {
                        traverseRootTerm(term, visitor);
                    }
                    visitor.visit(")"); //$NON-NLS-1$
                    break;
            }
        }
    }

    private final void fillTable(final List<TestCase> testCases,
            final RootTerm rootTerm) {
        if (testCases == null || rootTerm == null || testCases.size() == 0) {
            refreshAndPackTable(null);
            return;
        }

        Map<BooleanAssignment, Boolean> assignmentsMap = new HashMap<BooleanAssignment, Boolean>();

        // Merge the assignments of all the test cases.
        for (TestCase testCase : testCases) {
            assignmentsMap.putAll(testCase.getAssignments(rootTerm));
        }

        final TermCoverage termCoverage = TermCoverage.getInstance();
        

        List<RowObject> rows = new LinkedList<RowObject>();
        for (BooleanAssignment assignment : assignmentsMap.keySet()) {
            Map<BooleanTerm, CellObject> cells = new HashMap<BooleanTerm, CellObject>();
            CellObject resultCell;
            CellObject testCaseCell;
            
            Map<BooleanTerm, BooleanResult> wirksamMapT = new HashMap<BooleanTerm, BooleanResult>();
            Map<BooleanTerm, BooleanResult> wirksamMapF = new HashMap<BooleanTerm, BooleanResult>();

            Map<BooleanTerm, BooleanResult> termResults = new HashMap<BooleanTerm, BooleanResult>();
            termCoverage.evaluateTermWirksamkeit(rootTerm, rootTerm.getTerm(), assignment, termResults, wirksamMapT, wirksamMapF);
            

            Set<BooleanTerm> allTerms = termResults.keySet();
            for(BooleanTerm term : allTerms) {
            	if(term instanceof BasicBooleanTerm) {
                    Color coveredColor = null;

                    if (wirksamMapT.get(term) != null || wirksamMapF.get(term) != null) {
                        coveredColor = getCoveredBackground();
                    }

                    // Store the new CellObject in the cells map for the current
                    // BooleanTerm.
                    cells.put(term, new CellObject(
                            getStringFromBooleanResult(termResults.get(term)), null,
                            coveredColor));
            		
            	} else {
                    // Store the new CellObject in the cells map for the current
                    // BooleanTerm.
                    cells.put(term, new CellObject(
                            getStringFromBooleanResult(termResults.get(term)),
                            getOperatorForeground(), null));
            		
            	}
            }
                  

            // Add result cell:
            resultCell = new CellObject(
                    assignmentsMap.get(assignment) ? "1" : "0", null, null); //$NON-NLS-1$//$NON-NLS-2$

            // Add test case cell:
            StringBuilder sb = new StringBuilder();
            for (TestCase testCase : testCases) {
                BooleanAssignmentMap countMap = testCase
                        .getAssignmentsCount(rootTerm);

                long count = countMap.get(assignment);

                if (count != 0) {
                    if (sb.length() != 0) {
                        sb.append(", "); //$NON-NLS-1$
                    }
                    sb.append(testCase.getName());
                    sb.append(" ("); //$NON-NLS-1$
                    sb.append(count);
                    sb.append(")"); //$NON-NLS-1$
                }
            }
            
            int allTermCoverageCount = rootTerm.getTerm().getBasicBooleanTerms() * 2;
            int covered = wirksamMapT.size() + wirksamMapF.size();
            
            float coverage = 0;
            if(allTermCoverageCount != 0) {
            	coverage = covered * 100 / allTermCoverageCount;
            }
            
            DecimalFormat df = new DecimalFormat("0.0"); //$NON-NLS-1$
            sb.append(" Coverage: " + df.format(coverage));
            
            testCaseCell = new CellObject(sb.toString(), null, null);

            rows.add(new RowObject(cells, resultCell, testCaseCell));
        }

        refreshAndPackTable(rows.toArray());

        // update the status bar;
        CoverageResult result = termCoverage.getCoverage(testCases, rootTerm);
        double coveredItems = result.getCoveredItems();
        double totalItems = result.getTotalItems();
        double coverage;
        if (totalItems == 0.0) {
            coverage = 1.0;
        } else {
            coverage = coveredItems / totalItems;
        }
        DecimalFormat df = new DecimalFormat("0.0"); //$NON-NLS-1$
        this.statusBar.setText("term coverage for all test cases: " + df
                .format(100 * coverage) + " %");
    }
    
    Map<BooleanTerm, BooleanResult> termResults;
    

    /**
     * Evaluates the given {@link RootTerm} and its {@link BooleanTerm}s under
     * the given assignment, by traversing the expression tree. The cells are
     * filled with their data during that traversing.
     * 
     * @param rootTerm
     *            the {@link RootTerm}, the {@link BooleanTerm} belongs to.
     * @param booleanTerm
     *            the {@link BooleanTerm}, that is currently evaluated.
     * @param assignment
     *            the {@link BooleanAssignment}, that is used to evaluate.
     * @param cells
     *            the {@link Map} of {@link CellObject}s, that is filled.
     * @param coveringAssignments
     *            the {@link Map} of those {@link BooleanAssignment}s, that
     *            cover a given {@link BasicBooleanTerm}.
     * @return The {@link BooleanResult} of the evaluation of the given
     *         {@link BooleanTerm}.
     */
    private final BooleanResult evaluateTerm(RootTerm rootTerm,
            BooleanTerm booleanTerm, BooleanAssignment assignment,
            Map<BooleanTerm, CellObject> cells,
            Map<BasicBooleanTerm, Set<BooleanAssignment>> coveringAssignments) {

        if (booleanTerm instanceof BasicBooleanTerm) {
            int position = rootTerm.getPositionOfTerm((BasicBooleanTerm) booleanTerm);

            // Get the BooleanResult, that is at the position of the
            // BasicBooleanTerm in the given BooleanAssignment.
            BooleanResult booleanResult = assignment.getResults().get(position);
            Color coveredColor = null;

            // Set the background color of the cell to the color for coverage,
            // if the BasicBooleanTerm is covered by the given
            // BooleanAssignment.
            if (coveringAssignments.get(booleanTerm).contains(assignment)) {
                coveredColor = getCoveredBackground();
            }

            // Store the new CellObject in the cells map for the current
            // BooleanTerm.
            cells.put(booleanTerm, new CellObject(
                    getStringFromBooleanResult(booleanResult), null,
                    coveredColor));

            termResults.put(booleanTerm, booleanResult);

            return booleanResult;
            
        } else if (booleanTerm instanceof OperatorTerm) {
        	
        	OperatorTerm operatorTerm = (OperatorTerm)booleanTerm;

            List<BooleanResult> operatorResults = new LinkedList<BooleanResult>();
            
                                
            // Evaluate all the operands of this OperatorTerm and store the
            // results in a list.            
            for (BooleanTerm subTerms : operatorTerm.getOperands()) {
            	   
            	BooleanResult result = evaluateTerm(rootTerm, subTerms, assignment, cells, coveringAssignments);
	            operatorResults.add(result);
            }

            // Retrieve the Boolean result produced by the assignment of the
            // operands.
            BooleanAssignment operatorAssignment = new BooleanAssignment(operatorResults);
            Boolean result = ((OperatorTerm) booleanTerm).getOperator()
                    .getPossibleAssignments().get(operatorAssignment);
            

            // Convert into BooleanResult.
            BooleanResult booleanResult;
            if (result == null) {
                booleanResult = BooleanResult.NOT_EVALUATED;
            } else {
                booleanResult = result ? BooleanResult.TRUE
                        : BooleanResult.FALSE;
            }

            // Store the new CellObject in the cells map for the current
            // BooleanTerm.
            cells.put(booleanTerm, new CellObject(
                    getStringFromBooleanResult(booleanResult),
                    getOperatorForeground(), null));

            
            termResults.put(booleanTerm, booleanResult);

            return booleanResult;
        }

        return BooleanResult.NOT_EVALUATED;
    }


    
    private final void evaluateTermWirksamkeit(RootTerm rootTerm,
            BooleanTerm booleanTerm, BooleanAssignment assignment,
            Map<BooleanTerm, CellObject> cells) {

       }


    
    
    /**
     * Gets the {@link Color}, that is to be used for the text of the operator
     * results in the table.
     * 
     * @return the {@link Color} for the text of the operator results.
     */
    private final Color getOperatorForeground() {
        if (this.tableViewer == null) {
            return null;
        }

        return this.tableViewer.getTable().getDisplay().getSystemColor(
                SWT.COLOR_GRAY);
    }

    /**
     * Gets the {@link Color}, that is to be used for the background of the
     * cells of covered {@link BasicBooleanTerm}s.
     * 
     * @return the {@link Color} for the covered cells.
     */
    private final Color getCoveredBackground() {
        if (this.tableViewer == null) {
            return null;
        }

        return this.tableViewer.getTable().getDisplay().getSystemColor(
                SWT.COLOR_GREEN);
    }

    /**
     * Gets the string representation of a boolean result.
     * 
     * @param booleanResult
     *            the {@link BooleanResult} to be converted to a string
     * @return the string representation of a {@link BooleanResult}
     */
    private final String getStringFromBooleanResult(BooleanResult booleanResult) {
        String text;
        switch (booleanResult) {
            case FALSE:
                text = "F"; //$NON-NLS-1$
                break;
            case TRUE:
                text = "T"; //$NON-NLS-1$
                break;
            case NOT_EVALUATED:
                text = "x"; //$NON-NLS-1$
                break;
            default:
                text = ""; //$NON-NLS-1$
        }
        return text;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
     */
    @Override
    public void setFocus() {
        this.tableViewer.getTable().setFocus();
    }

    /**
     * Refreshes the {@link TableViewer} with the given input and packs the
     * columns.
     * 
     * @param input
     *            the new input for the viewer.
     */
    private final void refreshAndPackTable(Object input) {
        this.tableViewer.setInput(input);
        this.tableViewer.refresh();

        for (TableColumn column : this.tableViewer.getTable().getColumns()) {
            column.pack();
        }
    }

    /**
     * Handles the change to a new {@link TestSessionContainer}
     * 
     * @param testSessionContainer
     *            the new {@link TestSessionContainer}
     */
    private final void onTSCChanged(TestSessionContainer testSessionContainer) {
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();
        fillClassList(testSessionContainer);
        fillClassComboBox();
        fillConditionList(getSelectedClass());
        fillConditionComboBox();
        createTableColumns(getSelectedRootTerm());
        if (activeTSCInfo != null) {
            fillTable(new LinkedList<TestCase>(activeTSCInfo
                    .getActiveTestCases()), getSelectedRootTerm());
        } else {
            fillTable(new LinkedList<TestCase>(), getSelectedRootTerm());
        }
    }

    /**
     * Handles the change of active status of the {@link TestCase}s in the
     * current {@link TestSessionContainer}
     * 
     * @param activeTestCases
     *            the {@link Set} of active {@link TestCase}s
     */
    private final void onActiveTestCasesChanged(Set<TestCase> activeTestCases) {
        fillTable(new LinkedList<TestCase>(activeTestCases),
                getSelectedRootTerm());
    }

    /**
     * Displays the given {@link RootTerm} in the {@link BooleanAnalyzer}. <br>
     * Note: The {@link RootTerm} must be part of the given
     * {@link HierarchyLevel}. If it is not, then nothing will be done.
     * 
     * @param classLevel
     *            the {@link HierarchyLevel} representing the class the
     *            condition is part of.
     * @param rootTerm
     *            the {@link RootTerm} representing the to be displayed
     *            condition.
     */
    public void displayRootTerm(HierarchyLevel classLevel, RootTerm rootTerm) {
        ActiveTSContainerInfo activeTSCInfo = CodeCoverPlugin.getDefault()
                .getTSContainerManager().getActiveTSContainer();

        if (!this.classList.contains(classLevel)) {
            return;
        }

        // Select the given class in the ComboBox.
        this.classCombo.select(this.classList.indexOf(classLevel));
        this.fillConditionList(classLevel);

        // If the condition was not contained in the given class, we do nothing
        // further.
        if (!this.conditionList.contains(rootTerm)) {
            return;
        }

        this.conditionCombo.setRedraw(false);
        this.fillConditionComboBox();
        // If the condition was not contained in the given class, we display the
        // default RootTerm.
        if (this.conditionList.contains(rootTerm)) {
            this.conditionCombo.select(this.conditionList.indexOf(rootTerm));
        }
        this.conditionCombo.setRedraw(true);

        this.tableViewer.getTable().setRedraw(false);

        createTableColumns(getSelectedRootTerm());

        if (activeTSCInfo != null) {
            fillTable(new LinkedList<TestCase>(activeTSCInfo
                    .getActiveTestCases()), getSelectedRootTerm());
        } else {
            fillTable(new LinkedList<TestCase>(), getSelectedRootTerm());
        }

        this.tableViewer.getTable().setRedraw(true);
    }

    private final class TableContentProvider implements
            IStructuredContentProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#dispose()
         */
        @Override
		public void dispose() {
            // Do nothing here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer,
         *      java.lang.Object, java.lang.Object)
         */
        @Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
            // Do nothing here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
         */
        @Override
		public Object[] getElements(Object inputElement) {
            return (Object[]) inputElement;
        }
    }

    private final class TableLabelAndColorProvider implements
            ITableLabelProvider, ITableColorProvider {

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object,
         *      int)
         */
        @Override
		public Image getColumnImage(Object element, int columnIndex) {
            return null;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object,
         *      int)
         */
        @Override
		public String getColumnText(Object element, int columnIndex) {
            TableColumn column = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumn(columnIndex);

            int columnCount = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumnCount();
            BooleanTerm booleanTerm = (BooleanTerm) column
                    .getData(COLUMN_STORE_KEY);

            if (columnIndex == columnCount - 2) {
                return ((RowObject) element).resultCell.text;
            }

            if (columnIndex == columnCount - 1) {
                return ((RowObject) element).testCaseCell.text;
            }

            if (booleanTerm == null) {
                // Nothing was stored, so this is either a bracket column, or a
                // column for other inactive elements.
                return ""; //$NON-NLS-1$
            }

            return ((RowObject) element).cells.get(booleanTerm).text;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#addListener(org.eclipse.jface.viewers.ILabelProviderListener)
         */
        @Override
		public void addListener(ILabelProviderListener listener) {
            // Do nothing here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#dispose()
         */
        @Override
		public void dispose() {
            CodeCoverPlugin.getDefault().setBooleanAnalyzer(null);
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#isLabelProperty(java.lang.Object,
         *      java.lang.String)
         */
        @Override
		public boolean isLabelProperty(Object element, String property) {
            return false;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.IBaseLabelProvider#removeListener(org.eclipse.jface.viewers.ILabelProviderListener)
         */
        @Override
		public void removeListener(ILabelProviderListener listener) {
            // Do nothing here.
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object,
         *      int)
         */
        @Override
		public Color getBackground(Object element, int columnIndex) {
            TableColumn column = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumn(columnIndex);

            int columnCount = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumnCount();
            BooleanTerm booleanTerm = (BooleanTerm) column
                    .getData(COLUMN_STORE_KEY);

            if (columnIndex == columnCount - 2) {
                return ((RowObject) element).resultCell.backgroundColor;
            }

            if (columnIndex == columnCount - 1) {
                return ((RowObject) element).testCaseCell.backgroundColor;
            }

            if (booleanTerm == null) {
                // Nothing was stored, so this is either a bracket column, or a
                // column for other inactive elements.
                return null;
            }

            return ((RowObject) element).cells.get(booleanTerm).backgroundColor;
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object,
         *      int)
         */
        @Override
		public Color getForeground(Object element, int columnIndex) {
            TableColumn column = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumn(columnIndex);

            int columnCount = BooleanAnalyzer.this.tableViewer.getTable()
                    .getColumnCount();
            BooleanTerm booleanTerm = (BooleanTerm) column
                    .getData(COLUMN_STORE_KEY);

            if (columnIndex == columnCount - 2) {
                return ((RowObject) element).resultCell.foregroundColor;
            }

            if (columnIndex == columnCount - 1) {
                return ((RowObject) element).testCaseCell.foregroundColor;
            }

            if (booleanTerm == null) {
                // Nothing was stored, so this is either a bracket column, or a
                // column for other inactive elements.
                return null;
            }

            return ((RowObject) element).cells.get(booleanTerm).foregroundColor;
        }
    }

    private final class TSManagerListener implements TSContainerManagerListener {

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCaseChanged(org.codecover.eclipse.tscmanager.ActiveTSContainerInfo,
         *      org.codecover.model.utils.ChangeType,
         *      org.codecover.model.TestCase)
         */
        @Override
		public void testCaseChanged(final ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestCase testCase) {
            getSite().getShell().getDisplay().asyncExec(new Runnable() {
                @Override
				public void run() {
                    onActiveTestCasesChanged(tscInfo.getActiveTestCases());
                }
            });
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testCasesActivated(org.codecover.eclipse.tscmanager.ActiveTSContainerInfo)
         */
        @Override
		public void testCasesActivated(final ActiveTSContainerInfo tscInfo) {
            getSite().getShell().getDisplay().asyncExec(new Runnable() {
                @Override
				public void run() {
                    onActiveTestCasesChanged(tscInfo.getActiveTestCases());
                }
            });
        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionChanged(org.codecover.eclipse.tscmanager.ActiveTSContainerInfo,
         *      org.codecover.model.utils.ChangeType,
         *      org.codecover.model.TestSession)
         */
        @Override
		public void testSessionChanged(ActiveTSContainerInfo tscInfo,
                ChangeType changeType, TestSession testSession) {
            // We don't react on this.

        }

        /*
         * (non-Javadoc)
         * 
         * @see org.codecover.eclipse.tscmanager.TSContainerManagerListener#testSessionContainerActivated(org.codecover.eclipse.tscmanager.ActiveTSContainerInfo)
         */
        @Override
		public void testSessionContainerActivated(
                final ActiveTSContainerInfo tscInfo) {
            getSite().getShell().getDisplay().asyncExec(new Runnable() {
                @Override
				public void run() {
                    if (tscInfo != null) {
                        onTSCChanged(tscInfo.getTestSessionContainer());
                    } else {
                        onTSCChanged(null);
                    }
                }
            });
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

    private interface Visitor {
        /**
         * Visits a {@link BasicBooleanTerm}
         * 
         * @param booleanTerm
         *            the visited {@link BasicBooleanTerm}
         */
        public void visit(BasicBooleanTerm booleanTerm);

        /**
         * Visits a {@link OperatorTerm}
         * 
         * @param operatorTerm
         *            the visited {@link OperatorTerm}
         */
        public void visit(OperatorTerm operatorTerm);

        /**
         * Visits a {@link OperatorTerm} with an given name for the
         * {@link OperatorTerm}
         * 
         * @param operatorTerm
         *            the visited {@link OperatorTerm}
         * @param name
         *            the name of the {@link OperatorTerm}
         */
        public void visit(OperatorTerm operatorTerm, String name);

        /**
         * Visit method, which only gives a name for elements in the expression,
         * that is traversed, that do not correspond to either a
         * {@link BasicBooleanTerm} or an {@link OperatorTerm}.
         * 
         * @param name
         *            the name of the element.
         */
        public void visit(String name);
    }

    private final class CellObject {
        final String text;

        final Color foregroundColor;

        final Color backgroundColor;

        /**
         * Constructor
         * 
         * @param text
         * @param foregroundColor
         * @param backgroundColor
         */
        public CellObject(String text, Color foregroundColor,
                Color backgroundColor) {
            this.text = text;
            this.foregroundColor = foregroundColor;
            this.backgroundColor = backgroundColor;
        }
    }

    private final class RowObject {
        final Map<BooleanTerm, CellObject> cells;

        final CellObject resultCell;

        final CellObject testCaseCell;

        /**
         * Constructor
         * 
         * @param cells
         * @param resultCell
         * @param testCaseCell
         */
        public RowObject(Map<BooleanTerm, CellObject> cells,
                CellObject resultCell, CellObject testCaseCell) {
            this.cells = new HashMap<BooleanTerm, CellObject>(cells);
            this.resultCell = resultCell;
            this.testCaseCell = testCaseCell;
        }
    }
}
