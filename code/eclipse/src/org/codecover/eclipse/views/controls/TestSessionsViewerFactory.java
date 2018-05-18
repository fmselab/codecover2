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

import java.text.Collator;
import java.text.DateFormat;
import java.util.Date;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.eclipse.exportWizards.MainExportPage;
import org.codecover.eclipse.views.TestSessionsView;
import org.codecover.model.TestCase;
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.eclipse.jface.layout.TreeColumnLayout;
import org.eclipse.jface.viewers.CheckboxTreeViewer;
import org.eclipse.jface.viewers.ColumnWeightData;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TreeColumn;
import org.eclipse.ui.dialogs.ContainerCheckedTreeViewer;

/**
 * Produces <code>TreeViewer</code>s which list the test elements (test sessions
 * and test cases) of a test session container or an array of
 * <code>Object</code>s, which contains test elements of a test session
 * container.
 * <p>
 * If you want to set the layout data of a <code>TreeViewer</code> obtained from
 * this factory, use
 * <code>viewer.getTree().getParent().setLayoutData(Object)</code>.
 * <p>
 * When using the update method of a viewer, it is crucial to pass the correct
 * update properties (e.g., {@link #UPDATE_PROPERTY_NAME}), else the labels or
 * the sorting won't be updated correctly.
 * 
 * @see TestSessionsView
 * @see MergeWizardPage
 * @see MainExportPage
 * 
 * @author Robert Hanussek
 * @version 1.0 ($Id: TestSessionsViewerFactory.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TestSessionsViewerFactory {

    /**
     * The property passed to the update method of the tree viewer, if the name
     * of a test element changed.
     */
    public static final String UPDATE_PROPERTY_NAME = "name";      //$NON-NLS-1$

    /**
     * The property passed to the update method of the tree viewer, if the
     * comment of a test element changed.
     */
    public static final String UPDATE_PROPERTY_COMMENT = "comment";//$NON-NLS-1$

    /**
     * The property passed to the update method of the tree viewer, if the
     * date and time of a test element changed. (Currently the model of
     * CodeCover doesn't allow this, but it may be possible in future versions.)
     */
    public static final String UPDATE_PROPERTY_DATE = "date";      //$NON-NLS-1$

    private static final String TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION = Messages
            .getString("TestSessionsViewerFactory.TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION"); //$NON-NLS-1$

    private static final String COLUMN_LABEL_NAME = Messages
            .getString("TestSessionsViewerFactory.COLUMN_LABEL_NAME"); //$NON-NLS-1$

    private static final String COLUMN_LABEL_TEST_SESSION = Messages
            .getString("TestSessionsViewerFactory.COLUMN_LABEL_TEST_SESSION"); //$NON-NLS-1$

    private static final String COLUMN_LABEL_DATE = Messages
            .getString("TestSessionsViewerFactory.COLUMN_LABEL_DATE"); //$NON-NLS-1$

    private static final String COLUMN_LABEL_TIME = Messages
            .getString("TestSessionsViewerFactory.COLUMN_LABEL_TIME"); //$NON-NLS-1$

    private static final int STYLE
            = SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER;

    private static enum Columns {
        /**
         * Constant for the label of the name column
         */
        NAME(0, COLUMN_LABEL_NAME),
        /**
         * Constant for the label of the test session column
         */
        TESTSESSION(1, COLUMN_LABEL_TEST_SESSION),
        /**
         * Constant for the label of the date column
         */
        DATE(2, COLUMN_LABEL_DATE),
        /**
         * Constant for the label of the time column
         */
        TIME(3, COLUMN_LABEL_TIME);

        private final int index;
        private final String text;

        private Columns(int index, String text) {
            this.index = index;
            this.text = text;
        }

        /**
         * Returns the index of this column.
         * 
         * @param withTestSessionColumn <code>true</code>, if the test session
         *                              column is contained in the viewer,
         *                              <code>false</code> otherwise
         * 
         * @return  the index of this column
         */
        public int getIndex(boolean withTestSessionColumn) {
            if(!withTestSessionColumn && this == Columns.TESTSESSION) {
                throw new IllegalArgumentException();
            }

            if(!withTestSessionColumn
                    && this.index > Columns.TESTSESSION.getIndex(true)) {
                return this.index-1;
            } else {
                return this.index;
            }
        }

        /**
         * Returns the label of this column.
         * 
         * @return  the label of the column
         */
        public String getText() {
            return this.text;
        }
    }

    /**
     * Creates a new test sessions viewer as a
     * <code>CheckboxTreeViewer</code>.
     * 
     * @param parent                the composite on which the
     *                              <code>CheckboxTreeViewer</code> is to be
     *                              created
     * @param withTestSessionColumn <code>true</code>, if the test session
     *                              column is to be created, <code>false</code>
     *                              otherwise
     * @param disposeRunnable       this runnable is called, when the created
     *                              <code>CheckboxTreeViewer</code> is disposed
     * 
     * @return  a new test sessions viewer as a
     *          <code>CheckboxTreeViewer</code>
     */
    public static CheckboxTreeViewer newCheckboxTreeViewer(
                                                Composite parent,
                                                boolean withTestSessionColumn,
                                                Runnable disposeRunnable) {
        CheckboxTreeViewer viewer = new CheckboxTreeViewer(
                new Composite(parent, SWT.NONE),
                TestSessionsViewerFactory.STYLE);
        TestSessionsViewerFactory.configureViewer(  viewer,
                                                    withTestSessionColumn,
                                                    disposeRunnable);

        return viewer;
    }

    /**
     * Creates a new test sessions viewer as a
     * <code>ContainerCheckedTreeViewer</code>.
     * 
     * @param parent                the composite on which the
     *                              <code>ContainerCheckedTreeViewer</code> is
     *                              to be created
     * @param withTestSessionColumn <code>true</code>, if the test session
     *                              column is to be created, <code>false</code>
     *                              otherwise
     * @param disposeRunnable       this runnable is called, when the created
     *                              <code>ContainerCheckedTreeViewer</code> is
     *                              disposed
     * 
     * @return  a new test sessions viewer as a
     *          <code>ContainerCheckedTreeViewer</code>
     */
    public static ContainerCheckedTreeViewer newContainerCheckedTreeViewer(
                                                Composite parent,
                                                boolean withTestSessionColumn,
                                                Runnable disposeRunnable) {
        ContainerCheckedTreeViewer viewer = new ContainerCheckedTreeViewer(
                new Composite(parent, SWT.NONE),
                TestSessionsViewerFactory.STYLE);
        TestSessionsViewerFactory.configureViewer(  viewer,
                                                    withTestSessionColumn,
                                                    disposeRunnable);

        return viewer;
    }

    /**
     * Creates a new test sessions viewer as a <code>TreeViewer</code>.
     * 
     * @param parent                the composite on which the
     *                              <code>TreeViewer</code> is to be created
     * @param withTestSessionColumn <code>true</code>, if the test session
     *                              column is to be created, <code>false</code>
     *                              otherwise
     * @param disposeRunnable       this runnable is called, when the created
     *                              <code>TreeViewer</code> is disposed
     * 
     * @return  a new test sessions viewer as a <code>TreeViewer</code>
     */
    public static TreeViewer newTreeViewer( Composite parent,
                                            boolean withTestSessionColumn,
                                            Runnable disposeRunnable) {
        TreeViewer viewer = new TreeViewer(
                new Composite(parent, SWT.NONE),
                TestSessionsViewerFactory.STYLE);
        TestSessionsViewerFactory.configureViewer(  viewer,
                                                    withTestSessionColumn,
                                                    disposeRunnable);

        return viewer;
    }

    private static void configureViewer(    final TreeViewer viewer,
                                            final boolean withTestSessionColumn,
                                            final Runnable disposeRunnable) {
        TreeColumnLayout layout = new TreeColumnLayout();
        TreeColumn column;

        viewer.getTree().setRedraw(false);

        viewer.getTree().getParent().setLayout(layout);

        // set providers
        viewer.setContentProvider(new ViewerContentProvider(disposeRunnable));
        viewer.setLabelProvider(new ViewerLabelProvider(withTestSessionColumn));

        // add columns to viewer
        viewer.getTree().setHeaderVisible(true);
        // name column
        column = new TreeColumn(viewer.getTree(),
                                SWT.LEAD,
                                Columns.NAME.getIndex(withTestSessionColumn));
        column.setText(Columns.NAME.getText());
        column.setWidth(120);
        layout.setColumnData(column, new ColumnWeightData(52, 120));
        new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_NAME}) {
            private final ITableLabelProvider lblProvider
                    = (ITableLabelProvider)viewer.getLabelProvider();
            @Override
            protected int doCompare(Viewer viewer, Object o1, Object o2) {
                return Collator.getInstance().compare(
                        this.lblProvider.getColumnText(o1,
                                Columns.NAME.getIndex(withTestSessionColumn)),
                        this.lblProvider.getColumnText(o2,
                                Columns.NAME.getIndex(withTestSessionColumn)));
            }
        };
        // test session column (optional)
        if(withTestSessionColumn) {
            column = new TreeColumn(viewer.getTree(),
                    SWT.LEAD,
                    Columns.TESTSESSION.getIndex(withTestSessionColumn));
            column.setText(Columns.TESTSESSION.getText());
            column.setWidth(100);
            layout.setColumnData(column, new ColumnWeightData(16, 100));
            new ViewerSorter(viewer, column,new String[]{UPDATE_PROPERTY_NAME}){
                private final ITableLabelProvider lblProvider
                        = (ITableLabelProvider)viewer.getLabelProvider();
                @Override
                protected int doCompare(Viewer viewer, Object o1, Object o2) {
                    return Collator.getInstance().compare(
                            this.lblProvider.getColumnText(o1,
                                    Columns.TESTSESSION
                                            .getIndex(withTestSessionColumn)),
                            this.lblProvider.getColumnText(o2,
                                    Columns.TESTSESSION
                                            .getIndex(withTestSessionColumn)));
                }
            };
        }
        // date column
        column = new TreeColumn(viewer.getTree(),
                                SWT.LEAD,
                                Columns.DATE.getIndex(withTestSessionColumn));
        column.setText(Columns.DATE.getText());
        column.setWidth(80);
        layout.setColumnData(column, new ColumnWeightData(16, 80));
        new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {
            private final DateComparator comparator = new DateComparator();
            @Override
            protected int doCompare(Viewer viewer, Object o1, Object o2) {
                return this.comparator.compare(viewer, o1, o2);
            }
        };
        // time column
        column = new TreeColumn(viewer.getTree(),
                                SWT.LEAD,
                                Columns.TIME.getIndex(withTestSessionColumn));
        column.setText(Columns.TIME.getText());
        column.setWidth(60);
        layout.setColumnData(column, new ColumnWeightData(16, 60));
        new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {
            private final DateComparator comparator = new DateComparator();
            @Override
            protected int doCompare(Viewer viewer, Object o1, Object o2) {
                return this.comparator.compare(viewer, o1, o2);
            }
        };

        viewer.getTree().setRedraw(true);
    }

    private static final class ViewerContentProvider
            implements ITreeContentProvider {

        private Runnable disposeRunnable;

        /**
         * Constructs a content provider which provides the content a test
         * session container (its test sessions and test cases) or the content
         * of an array of <code>Object</code>s, which contains test sessions and
         * test cases of a test session container.
         * 
         * @param disposeRunnable   this runnable is called, when the
         *                          corresponding viewer is disposed
         */
        public ViewerContentProvider(Runnable disposeRunnable) {
            this.disposeRunnable = disposeRunnable;
        }

        @Override
		public void inputChanged(Viewer v, Object oldInput, Object newInput) {}

        @Override
		public void dispose() {
            if(this.disposeRunnable != null) {
                this.disposeRunnable.run();
            }
        }

        // is only called for the (invisible) root (the setInput argument)
        @Override
		public Object[] getElements(Object parent) {
            /*
             * if you change this method, apply the according changes to methods
             * getChildren and hasChildren, too
             */

            if(parent instanceof TestSessionContainer) {
                return ((TestSessionContainer)parent).getTestSessions()
                                                     .toArray();
            } else if(parent instanceof Object[]) {
                return (Object[])parent;
            } else {
                throw new RuntimeException("unknown input set");   //$NON-NLS-1$
            }
        }

        @Override
		public Object[] getChildren(Object parent) {
            if(parent instanceof TestCase) {
                return new Object[0];
            } else if(parent instanceof TestSession) {
                return ((TestSession)parent).getTestCases().toArray();
            }
            /*
             * if you change the following two branches, apply the according
             * changes to methods hasChildren and getElements, too
             */
            else if(parent instanceof TestSessionContainer) {
                return ((TestSessionContainer)parent).getTestSessions()
                                                     .toArray();
            } else if(parent instanceof Object[]) {
                return (Object[])parent;
            } else {
                throw new RuntimeException(
                        "unknown element in tree viewer?! "        //$NON-NLS-1$
                        + parent.toString()
                        + " class: " + parent.getClass());         //$NON-NLS-1$
            }
        }

        @Override
		public boolean hasChildren(Object parent) {
            if(parent instanceof TestCase) {
                return false;
            } else if(parent instanceof TestSession) {
                return !((TestSession)parent).getTestCases().isEmpty();
            }
            /*
             * if you change the following two branches, apply the according
             * changes to methods getChildren and getElements, too
             */
            else if(parent instanceof TestSessionContainer) {
                return !((TestSessionContainer)parent).getTestSessions()
                                                      .isEmpty();
            } else if(parent instanceof Object[]) {
                return ((Object[])parent).length > 0;
            } else {
                throw new RuntimeException(
                        "unknown element in tree viewer?! "        //$NON-NLS-1$
                        + parent.toString()
                        + " class: " + parent.getClass());         //$NON-NLS-1$
            }
        }


        @Override
		public Object getParent(Object child) {
            if(child instanceof TestSessionContainer
                    || child instanceof Object[]) {
                return null;
            } else if(child instanceof TestSession) {
                return ((TestSession)child).getTestSessionContainer();
            } else if(child instanceof TestCase) {
                return ((TestCase)child).getTestSession();
            } else {
                throw new RuntimeException(
                        "unknown element in tree viewer?! "        //$NON-NLS-1$
                        + child.toString()
                        + " class: " + child.getClass());          //$NON-NLS-1$
            }
        }

    }

    private static final class ViewerLabelProvider extends LabelProvider
            implements ITableLabelProvider {

        private final boolean withTestSessionColumn;

        /**
         * Constructs a label provider which can provide labels and icons for
         * test sessions and test cases.
         * 
         * @param withTestSessionColumn <code>true</code>, if the test session
         *                              column is contained in the corresponding
         *                              viewer, <code>false</code> otherwise
         */
        public ViewerLabelProvider(boolean withTestSessionColumn) {
            this.withTestSessionColumn = withTestSessionColumn;
        }

        @Override
		public String getColumnText(Object element, int index) {
            if(element instanceof TestSession) {
                if(index == Columns.NAME
                        .getIndex(this.withTestSessionColumn)) {
                    return ((TestSession)element).getName();
                } else if(this.withTestSessionColumn &&
                        index == Columns.TESTSESSION
                                .getIndex(this.withTestSessionColumn)) {
                    return TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION;
                } else if(index == Columns.DATE
                        .getIndex(this.withTestSessionColumn)) {
                    return DateFormat.getDateInstance()
                                     .format(((TestSession)element).getDate());
                } else if(index == Columns.TIME
                        .getIndex(this.withTestSessionColumn)) {
                    return DateFormat.getTimeInstance()
                                     .format(((TestSession)element).getDate());
                } else {
                    return "";                                     //$NON-NLS-1$
                }
            } else if(element instanceof TestCase) {
                if(index == Columns.NAME
                        .getIndex(this.withTestSessionColumn)) {
                    return ((TestCase)element).getName();
                } else if(this.withTestSessionColumn &&
                        index == Columns.TESTSESSION
                                .getIndex(this.withTestSessionColumn)) {
                    return ((TestCase)element).getTestSession().getName();
                } else if(index == Columns.DATE
                        .getIndex(this.withTestSessionColumn)) {
                    return DateFormat.getDateInstance()
                                     .format(((TestCase)element).getDate());
                } else if(index == Columns.TIME
                        .getIndex(this.withTestSessionColumn)) {
                    return DateFormat.getTimeInstance()
                                     .format(((TestCase)element).getDate());
                } else {
                    return "";                                     //$NON-NLS-1$
                }
            } else {
                throw new RuntimeException(
                        "unknown element in tree viewer?! "        //$NON-NLS-1$
                        + element.toString()
                        + " class: " + element.getClass());        //$NON-NLS-1$
            }
        }

        @Override
		public Image getColumnImage(Object element, int index) {
            if(index == Columns.NAME.getIndex(this.withTestSessionColumn)) {
                if(element instanceof TestSession) {
                    return CodeCoverPlugin.getDefault().getImageRegistry().get(
                            CodeCoverPlugin.Image.TEST_SESSION.getPath());
                } else if(element instanceof TestCase) {
                    return CodeCoverPlugin.getDefault().getImageRegistry().get(
                            CodeCoverPlugin.Image.TEST_CASE.getPath());
                } else {
                    throw new RuntimeException(
                            "unknown element in tree viewer?! "    //$NON-NLS-1$
                            + element.toString()
                            + " class: " + element.getClass());    //$NON-NLS-1$
                }
            } else {
                return null;
            }
        }

        @Override
        public boolean isLabelProperty(Object element, String property) {
            return UPDATE_PROPERTY_NAME.equals(property)
                || UPDATE_PROPERTY_DATE.equals(property);
        }

    }

    private static abstract class ViewerSorter extends ViewerComparator {

        public static final int SORTORDER_ASC = 1;
        public static final int SORTORDER_DESC = -1;
        public static final int SORTORDER_INIT = SORTORDER_ASC;

        private final TreeViewer viewer;
        private final TreeColumn column;
        private int direction;
        private final String[] updateProperties;

        public ViewerSorter(TreeViewer viewer, TreeColumn column,
                String[] updateProperties) {
            this.viewer = viewer;
            this.column = column;
            this.direction = SORTORDER_INIT;
            this.updateProperties = updateProperties;

            this.column.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    ViewerSorter thiz = ViewerSorter.this;
                    if(thiz.viewer.getComparator() == thiz) {
                        if(thiz.direction == SORTORDER_ASC){
                            thiz.becomeSorter(SORTORDER_DESC);
                        } else if(thiz.direction == SORTORDER_DESC) {
                            thiz.becomeSorter(SORTORDER_ASC);
                        }
                    } else {
                        thiz.becomeSorter(SORTORDER_INIT);
                    }
                }
            });
        }

        private void becomeSorter(int direction) {
                this.column.getParent().setSortColumn(this.column);
                this.direction = direction;

                if(this.direction == SORTORDER_ASC ) {
                    this.column.getParent().setSortDirection(SWT.UP);
                } else {
                    this.column.getParent().setSortDirection(SWT.DOWN);
                }

                if(this.viewer.getComparator() == this) {
                    this.viewer.refresh();
                } else {
                    this.viewer.setComparator(this);
                }
        }

        @Override
        public int compare(Viewer viewer, Object o1, Object o2) {
            return this.direction * doCompare(viewer, o1, o2);
        }

        protected abstract int doCompare(Viewer viewer, Object o1, Object o2);

        @Override
        public boolean isSorterProperty(Object element, String property) {
            for(String sensibleProperty : this.updateProperties) {
                if(sensibleProperty.equals(property)) {
                    return true;
                }
            }
            return false;
        }

    }

    private static final class DateComparator extends ViewerComparator {

        @Override
        public int compare(Viewer viewer, Object o1, Object o2) {
            Date d1 = null;
            Date d2 = null;
            if(o1 instanceof TestSession) {
                d1 = ((TestSession)o1).getDate();
            } else if(o1 instanceof TestCase) {
                d1 = ((TestCase)o1).getDate();
            }
            if(o2 instanceof TestSession) {
                d2 = ((TestSession)o2).getDate();
            } else if(o2 instanceof TestCase) {
                d2 = ((TestCase)o2).getDate();
            }
            if(d1 != null && d2 != null) {
                return d1.compareTo(d2);
            } else if(d1 != null) {
                return 1;
            } else {
                return -1;
            }
//            if(o1 instanceof TestSession && o2 instanceof TestSession) {
//                return ((TestSession)o1).getDate().compareTo(
//                        ((TestSession)o2).getDate());
//            } else if(o1 instanceof TestCase && o2 instanceof TestCase) {
//                return ((TestCase)o1).getDate().compareTo(
//                        ((TestCase)o2).getDate());
//            } else if(o1 instanceof TestSession) {
//                return -1;
//            } else {
//                return 1;
//            }
        }

    }

}
