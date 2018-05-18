/******************************************************************************
 * Copyright (c) 2009 Negar Koochakzadeh, Vahid Garousi			      *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.eclipse.views.controls;

import java.text.Collator;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
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
 * This {@link RedundancyViewerFactory} Project supervisor: Vahid Garousi (http://www.ucalgary.ca/~vgarousi/)
 * Software Quality Engineering Research Group (SoftQual) Department of Electrical and Computer Engineering
 * Schulich School of Engineering University of Calgary, Alberta, Canada http://www.softqual.ucalgary.ca
 *
 * @author Negar Koochakzadeh
 * @version 1.0
 */

public class RedundancyViewerFactory {

  /**
   * The property passed to the update method of the tree viewer, if the name of a test element changed.
   */
  public static final String UPDATE_PROPERTY_NAME = "name"; //$NON-NLS-1$

  /**
   * The property passed to the update method of the tree viewer, if the comment of a test element changed.
   */
  public static final String UPDATE_PROPERTY_COMMENT = "comment";//$NON-NLS-1$

  /**
   * The property passed to the update method of the tree viewer, if the date and time of a test element
   * changed. (Currently the model of CodeCover doesn't allow this, but it may be possible in future
   * versions.)
   */
  public static final String UPDATE_PROPERTY_DATE = "date"; //$NON-NLS-1$

  private static final String TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION =
    Messages.getString("TestSessionsViewerFactory.TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_NAME =
    Messages.getString("TestSessionsViewerFactory.COLUMN_LABEL_NAME"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_TEST_SESSION =
    Messages.getString("TestSessionsViewerFactory.COLUMN_LABEL_TEST_SESSION"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_REDUNDANCY =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_REDUNDANCY"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_ST_REDUNDANCY =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_ST_REDUNDANCY"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_BR_REDUNDANCY =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_BR_REDUNDANCY"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_COND_REDUNDANCY =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_COND_REDUNDANCY"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_LOOP_REDUNDANCY =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_LOOP_REDUNDANCY"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_COVEREDITEMS =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_COVEREDITEMS"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_ST_COVEREDITEMS =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_ST_COVEREDITEMS"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_BR_COVEREDITEMS =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_BR_COVEREDITEMS"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_COND_COVEREDITEMS =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_COND_COVEREDITEMS"); //$NON-NLS-1$

  private static final String COLUMN_LABEL_LOOP_COVEREDITEMS =
    Messages.getString("RedundancyViewerFactory.COLUMN_LABEL_LOOP_COVEREDITEMS"); //$NON-NLS-1$

  private static final int STYLE = SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL | SWT.BORDER;

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
    ST_REDUNDANCY(2, COLUMN_LABEL_ST_REDUNDANCY),
    BR_REDUNDANCY(3, COLUMN_LABEL_BR_REDUNDANCY),
    COND_REDUNDANCY(4, COLUMN_LABEL_COND_REDUNDANCY),
    LOOP_REDUNDANCY(5, COLUMN_LABEL_LOOP_REDUNDANCY),
    REDUNDANCY(6, COLUMN_LABEL_REDUNDANCY),
    /**
     * Constant for the label of the time column
     */
    ST_COVEREDITEMS(7, COLUMN_LABEL_ST_COVEREDITEMS),
    BR_COVEREDITEMS(8, COLUMN_LABEL_BR_COVEREDITEMS),
    COND_COVEREDITEMS(9, COLUMN_LABEL_COND_COVEREDITEMS),
    LOOP_COVEREDITEMS(10, COLUMN_LABEL_LOOP_COVEREDITEMS),
    COVEREDITEMS(11, COLUMN_LABEL_COVEREDITEMS);

    private final int index;

    private final String text;

    private Columns(int index, String text) {
      this.index = index;
      this.text = text;
    }

    /**
     * Returns the index of this column.
     *
     * @param withTestSessionColumn <code>true</code>, if the test session column is contained in the
     *        viewer, <code>false</code> otherwise
     * @return the index of this column
     */
    public int getIndex(boolean withTestSessionColumn) {
      if (!withTestSessionColumn && this == Columns.TESTSESSION) {
        throw new IllegalArgumentException();
      }

      if (!withTestSessionColumn && this.index > Columns.TESTSESSION.getIndex(true)) {
        return this.index - 1;
      } else {
        return this.index;
      }
    }

    /**
     * Returns the label of this column.
     *
     * @return the label of the column
     */
    public String getText() {
      return this.text;
    }
  }

  /**
   * Creates a new test sessions viewer as a <code>CheckboxTreeViewer</code>.
   *
   * @param parent the composite on which the <code>CheckboxTreeViewer</code> is to be created
   * @param withTestSessionColumn <code>true</code>, if the test session column is to be created,
   *        <code>false</code> otherwise
   * @param disposeRunnable this runnable is called, when the created <code>CheckboxTreeViewer</code> is
   *        disposed
   * @return a new test sessions viewer as a <code>CheckboxTreeViewer</code>
   */
  public static CheckboxTreeViewer newCheckboxTreeViewer(Composite parent, boolean withTestSessionColumn,
    Runnable disposeRunnable) {
    CheckboxTreeViewer viewer =
      new CheckboxTreeViewer(new Composite(parent, SWT.NONE), RedundancyViewerFactory.STYLE);
    RedundancyViewerFactory.configureViewer(viewer, withTestSessionColumn, disposeRunnable);

    return viewer;
  }

  /**
   * Creates a new test sessions viewer as a <code>ContainerCheckedTreeViewer</code>.
   *
   * @param parent the composite on which the <code>ContainerCheckedTreeViewer</code> is to be created
   * @param withTestSessionColumn <code>true</code>, if the test session column is to be created,
   *        <code>false</code> otherwise
   * @param disposeRunnable this runnable is called, when the created <code>ContainerCheckedTreeViewer</code>
   *        is disposed
   * @return a new test sessions viewer as a <code>ContainerCheckedTreeViewer</code>
   */
  public static ContainerCheckedTreeViewer newContainerCheckedTreeViewer(Composite parent,
    boolean withTestSessionColumn, Runnable disposeRunnable) {
    ContainerCheckedTreeViewer viewer =
      new ContainerCheckedTreeViewer(new Composite(parent, SWT.NONE), RedundancyViewerFactory.STYLE);
    RedundancyViewerFactory.configureViewer(viewer, withTestSessionColumn, disposeRunnable);

    return viewer;
  }

  /**
   * Creates a new test sessions viewer as a <code>TreeViewer</code>.
   *
   * @param parent the composite on which the <code>TreeViewer</code> is to be created
   * @param withTestSessionColumn <code>true</code>, if the test session column is to be created,
   *        <code>false</code> otherwise
   * @param disposeRunnable this runnable is called, when the created <code>TreeViewer</code> is disposed
   * @return a new test sessions viewer as a <code>TreeViewer</code>
   */
  public static TreeViewer newTreeViewer(Composite parent, boolean withTestSessionColumn,
    Runnable disposeRunnable) {
    TreeViewer viewer = new TreeViewer(new Composite(parent, SWT.NONE), RedundancyViewerFactory.STYLE);
    RedundancyViewerFactory.configureViewer(viewer, withTestSessionColumn, disposeRunnable);

    return viewer;
  }

  private static void configureViewer(final TreeViewer viewer, final boolean withTestSessionColumn,
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
    column = new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.NAME.getIndex(withTestSessionColumn));
    column.setText(Columns.NAME.getText());
    column.setWidth(120);
    layout.setColumnData(column, new ColumnWeightData(52, 120));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_NAME}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        return Collator.getInstance().compare(
          this.lblProvider.getColumnText(o1, Columns.NAME.getIndex(withTestSessionColumn)),
          this.lblProvider.getColumnText(o2, Columns.NAME.getIndex(withTestSessionColumn)));
      }
    };
    // test session column (optional)
    if (withTestSessionColumn) {
      column =
        new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.TESTSESSION.getIndex(withTestSessionColumn));
      column.setText(Columns.TESTSESSION.getText());
      column.setWidth(100);
      layout.setColumnData(column, new ColumnWeightData(16, 100));
      new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_NAME}) {

        private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

        @Override
        protected int doCompare(Viewer viewer, Object o1, Object o2) {
          return Collator.getInstance().compare(
            this.lblProvider.getColumnText(o1, Columns.TESTSESSION.getIndex(withTestSessionColumn)),
            this.lblProvider.getColumnText(o2, Columns.TESTSESSION.getIndex(withTestSessionColumn)));
        }
      };
    }
    // Redundancy columns:
    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.ST_REDUNDANCY.getIndex(withTestSessionColumn));
    column.setText(Columns.ST_REDUNDANCY.getText());
    column.setWidth(80);
    layout.setColumnData(column, new ColumnWeightData(16, 80));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        String str1 =
          this.lblProvider.getColumnText(o1, Columns.ST_REDUNDANCY.getIndex(withTestSessionColumn));
        String str2 =
          this.lblProvider.getColumnText(o2, Columns.ST_REDUNDANCY.getIndex(withTestSessionColumn));
        if (str1.compareTo("NaN") == 0) {
          return 1;
        } else if (str2.compareTo("NaN") == 0) {
          return -1;
        } else {
          double d1 = Double.parseDouble(str1);
          double d2 = Double.parseDouble(str2);
          if (d1 <= d2) {
            return 1;
          } else {
            return -1;
          }
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.BR_REDUNDANCY.getIndex(withTestSessionColumn));
    column.setText(Columns.BR_REDUNDANCY.getText());
    column.setWidth(80);
    layout.setColumnData(column, new ColumnWeightData(16, 80));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        String str1 =
          this.lblProvider.getColumnText(o1, Columns.BR_REDUNDANCY.getIndex(withTestSessionColumn));
        String str2 =
          this.lblProvider.getColumnText(o2, Columns.BR_REDUNDANCY.getIndex(withTestSessionColumn));
        if (str1.compareTo("NaN") == 0) {
          return 1;
        } else if (str2.compareTo("NaN") == 0) {
          return -1;
        } else {
          double d1 = Double.parseDouble(str1);
          double d2 = Double.parseDouble(str2);
          if (d1 <= d2) {
            return 1;
          } else {
            return -1;
          }
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.COND_REDUNDANCY.getIndex(withTestSessionColumn));
    column.setText(Columns.COND_REDUNDANCY.getText());
    column.setWidth(80);
    layout.setColumnData(column, new ColumnWeightData(16, 80));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        String str1 =
          this.lblProvider.getColumnText(o1, Columns.COND_REDUNDANCY.getIndex(withTestSessionColumn));
        String str2 =
          this.lblProvider.getColumnText(o2, Columns.COND_REDUNDANCY.getIndex(withTestSessionColumn));
        if (str1.compareTo("NaN") == 0) {
          return 1;
        } else if (str2.compareTo("NaN") == 0) {
          return -1;
        } else {
          double d1 = Double.parseDouble(str1);
          double d2 = Double.parseDouble(str2);
          if (d1 <= d2) {
            return 1;
          } else {
            return -1;
          }
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.LOOP_REDUNDANCY.getIndex(withTestSessionColumn));
    column.setText(Columns.LOOP_REDUNDANCY.getText());
    column.setWidth(80);
    layout.setColumnData(column, new ColumnWeightData(16, 80));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        String str1 =
          this.lblProvider.getColumnText(o1, Columns.LOOP_REDUNDANCY.getIndex(withTestSessionColumn));
        String str2 =
          this.lblProvider.getColumnText(o2, Columns.LOOP_REDUNDANCY.getIndex(withTestSessionColumn));
        if (str1.compareTo("NaN") == 0) {
          return 1;
        } else if (str2.compareTo("NaN") == 0) {
          return -1;
        } else {
          double d1 = Double.parseDouble(str1);
          double d2 = Double.parseDouble(str2);
          if (d1 <= d2) {
            return 1;
          } else {
            return -1;
          }
        }
      }
    };

    column = new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.REDUNDANCY.getIndex(withTestSessionColumn));
    column.setText(Columns.REDUNDANCY.getText());
    column.setWidth(80);
    layout.setColumnData(column, new ColumnWeightData(16, 80));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        String str1 = this.lblProvider.getColumnText(o1, Columns.REDUNDANCY.getIndex(withTestSessionColumn));
        String str2 = this.lblProvider.getColumnText(o2, Columns.REDUNDANCY.getIndex(withTestSessionColumn));
        if (str1.compareTo("NaN") == 0) {
          return 1;
        } else if (str2.compareTo("NaN") == 0) {
          return -1;
        } else {
          double d1 = Double.parseDouble(str1);
          double d2 = Double.parseDouble(str2);
          if (d1 <= d2) {
            return 1;
          } else {
            return -1;
          }
        }
      }
    };

    // Covered Items columns:
    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.ST_COVEREDITEMS.getIndex(withTestSessionColumn));
    column.setText(Columns.ST_COVEREDITEMS.getText());
    column.setWidth(60);
    layout.setColumnData(column, new ColumnWeightData(16, 60));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        int a =
          Integer.parseInt(this.lblProvider.getColumnText(o1, Columns.ST_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        int b =
          Integer.parseInt(this.lblProvider.getColumnText(o2, Columns.ST_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        if (a <= b) {
          return 1;
        } else {
          return -1;
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.BR_COVEREDITEMS.getIndex(withTestSessionColumn));
    column.setText(Columns.BR_COVEREDITEMS.getText());
    column.setWidth(60);
    layout.setColumnData(column, new ColumnWeightData(16, 60));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        int a =
          Integer.parseInt(this.lblProvider.getColumnText(o1, Columns.BR_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        int b =
          Integer.parseInt(this.lblProvider.getColumnText(o2, Columns.BR_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        if (a <= b) {
          return 1;
        } else {
          return -1;
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.COND_COVEREDITEMS.getIndex(withTestSessionColumn));
    column.setText(Columns.COND_COVEREDITEMS.getText());
    column.setWidth(60);
    layout.setColumnData(column, new ColumnWeightData(16, 60));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        int a =
          Integer.parseInt(this.lblProvider.getColumnText(o1, Columns.COND_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        int b =
          Integer.parseInt(this.lblProvider.getColumnText(o2, Columns.COND_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        if (a <= b) {
          return 1;
        } else {
          return -1;
        }
      }
    };

    column =
      new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.LOOP_COVEREDITEMS.getIndex(withTestSessionColumn));
    column.setText(Columns.LOOP_COVEREDITEMS.getText());
    column.setWidth(60);
    layout.setColumnData(column, new ColumnWeightData(16, 60));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        int a =
          Integer.parseInt(this.lblProvider.getColumnText(o1, Columns.LOOP_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        int b =
          Integer.parseInt(this.lblProvider.getColumnText(o2, Columns.LOOP_COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        if (a <= b) {
          return 1;
        } else {
          return -1;
        }
      }
    };

    column = new TreeColumn(viewer.getTree(), SWT.LEAD, Columns.COVEREDITEMS.getIndex(withTestSessionColumn));
    column.setText(Columns.COVEREDITEMS.getText());
    column.setWidth(60);
    layout.setColumnData(column, new ColumnWeightData(16, 60));
    new ViewerSorter(viewer, column, new String[] {UPDATE_PROPERTY_DATE}) {

      private final ITableLabelProvider lblProvider = (ITableLabelProvider) viewer.getLabelProvider();

      @Override
      protected int doCompare(Viewer viewer, Object o1, Object o2) {
        int a =
          Integer.parseInt(this.lblProvider.getColumnText(o1, Columns.COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        int b =
          Integer.parseInt(this.lblProvider.getColumnText(o2, Columns.COVEREDITEMS
            .getIndex(withTestSessionColumn)));
        if (a <= b) {
          return 1;
        } else {
          return -1;
        }
      }
    };

    viewer.getTree().setRedraw(true);
  }

  private static final class ViewerContentProvider
    implements ITreeContentProvider {

    private Runnable disposeRunnable;

    /**
     * Constructs a content provider which provides the content a test session container (its test sessions
     * and test cases) or the content of an array of <code>Object</code>s, which contains test sessions and
     * test cases of a test session container.
     *
     * @param disposeRunnable this runnable is called, when the corresponding viewer is disposed
     */
    public ViewerContentProvider(Runnable disposeRunnable) {
      this.disposeRunnable = disposeRunnable;
    }

    @Override
	public void inputChanged(Viewer v, Object oldInput, Object newInput) {
    }

    @Override
	public void dispose() {
      if (this.disposeRunnable != null) {
        this.disposeRunnable.run();
      }
    }

    // is only called for the (invisible) root (the setInput argument)
    @Override
	public Object[] getElements(Object parent) {
      /*
       * if you change this method, apply the according changes to methods getChildren and hasChildren, too
       */

      if (parent instanceof TestSessionContainer) {
        return ((TestSessionContainer) parent).getTestSessions().toArray();
      } else if (parent instanceof Object[]) {
        return (Object[]) parent;
      } else {
        throw new RuntimeException("unknown input set"); //$NON-NLS-1$
      }
    }

    @Override
	public Object[] getChildren(Object parent) {
      if (parent instanceof TestCase) {
        return new Object[0];
      } else if (parent instanceof TestSession) {
        return ((TestSession) parent).getTestCases().toArray();
      }
      /*
       * if you change the following two branches, apply the according changes to methods hasChildren and
       * getElements, too
       */
      else if (parent instanceof TestSessionContainer) {
        return ((TestSessionContainer) parent).getTestSessions().toArray();
      } else if (parent instanceof Object[]) {
        return (Object[]) parent;
      } else {
        throw new RuntimeException("unknown element in tree viewer?! " //$NON-NLS-1$
          + parent.toString() + " class: " + parent.getClass()); //$NON-NLS-1$
      }
    }

    @Override
	public boolean hasChildren(Object parent) {
      if (parent instanceof TestCase) {
        return false;
      } else if (parent instanceof TestSession) {
        return !((TestSession) parent).getTestCases().isEmpty();
      }
      /*
       * if you change the following two branches, apply the according changes to methods getChildren and
       * getElements, too
       */
      else if (parent instanceof TestSessionContainer) {
        return !((TestSessionContainer) parent).getTestSessions().isEmpty();
      } else if (parent instanceof Object[]) {
        return ((Object[]) parent).length > 0;
      } else {
        throw new RuntimeException("unknown element in tree viewer?! " //$NON-NLS-1$
          + parent.toString() + " class: " + parent.getClass()); //$NON-NLS-1$
      }
    }

    @Override
	public Object getParent(Object child) {
      if (child instanceof TestSessionContainer || child instanceof Object[]) {
        return null;
      } else if (child instanceof TestSession) {
        return ((TestSession) child).getTestSessionContainer();
      } else if (child instanceof TestCase) {
        return ((TestCase) child).getTestSession();
      } else {
        throw new RuntimeException("unknown element in tree viewer?! " //$NON-NLS-1$
          + child.toString() + " class: " + child.getClass()); //$NON-NLS-1$
      }
    }

  }

  private static final class ViewerLabelProvider
    extends LabelProvider
    implements ITableLabelProvider {

    private final boolean withTestSessionColumn;

    /**
     * Constructs a label provider which can provide labels and icons for test sessions and test cases.
     *
     * @param withTestSessionColumn <code>true</code>, if the test session column is contained in the
     *        corresponding viewer, <code>false</code> otherwise
     */
    public ViewerLabelProvider(boolean withTestSessionColumn) {
      this.withTestSessionColumn = withTestSessionColumn;
    }

    @Override
	public String getColumnText(Object element, int index) {
      if (element instanceof TestSession) {
        if (index == Columns.NAME.getIndex(this.withTestSessionColumn)) {
          return ((TestSession) element).getName();
        } else if (this.withTestSessionColumn
          && index == Columns.TESTSESSION.getIndex(this.withTestSessionColumn)) {
          return TESTSESSION_COLUMN_TEXT_OF_TEST_SESSION;
        } else {
          return ""; //$NON-NLS-1$
        }
      } else if (element instanceof TestCase) {
        if (index == Columns.NAME.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getName();
        } else if (this.withTestSessionColumn
          && index == Columns.TESTSESSION.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getTestSession().getName();
        } else if (index == Columns.ST_REDUNDANCY.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getSatementRedundancy().toString();
        } else if (index == Columns.BR_REDUNDANCY.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getBranchRedundancy().toString();
        } else if (index == Columns.COND_REDUNDANCY.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getCondRedundancy().toString();
        } else if (index == Columns.LOOP_REDUNDANCY.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getLoopRedundancy().toString();
        } else if (index == Columns.REDUNDANCY.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getTotalRedundancy().toString();
        } else if (index == Columns.ST_COVEREDITEMS.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getStatementCoveredItem().toString();
        } else if (index == Columns.BR_COVEREDITEMS.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getBranchCoveredItem().toString();
        } else if (index == Columns.COND_COVEREDITEMS.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getCondCoveredItem().toString();
        } else if (index == Columns.LOOP_COVEREDITEMS.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getLoopCoveredItem().toString();
        } else if (index == Columns.COVEREDITEMS.getIndex(this.withTestSessionColumn)) {
          return ((TestCase) element).getTotalCoveredItem().toString();
        } else {
          return ""; //$NON-NLS-1$
        }
      } else {
        throw new RuntimeException("unknown element in tree viewer?! " //$NON-NLS-1$
          + element.toString() + " class: " + element.getClass()); //$NON-NLS-1$
      }
    }

    @Override
	public Image getColumnImage(Object element, int index) {
      if (index == Columns.NAME.getIndex(this.withTestSessionColumn)) {
        if (element instanceof TestSession) {
          return CodeCoverPlugin.getDefault().getImageRegistry().get(
            CodeCoverPlugin.Image.TEST_SESSION.getPath());
        } else if (element instanceof TestCase) {
          return CodeCoverPlugin.getDefault().getImageRegistry().get(
            CodeCoverPlugin.Image.TEST_CASE.getPath());
        } else {
          throw new RuntimeException("unknown element in tree viewer?! " //$NON-NLS-1$
            + element.toString() + " class: " + element.getClass()); //$NON-NLS-1$
        }
      } else {
        return null;
      }
    }

    @Override
    public boolean isLabelProperty(Object element, String property) {
      return UPDATE_PROPERTY_NAME.equals(property) || UPDATE_PROPERTY_DATE.equals(property);
    }

  }

  private static abstract class ViewerSorter
    extends ViewerComparator {

    public static final int SORTORDER_ASC = 1;

    public static final int SORTORDER_DESC = -1;

    public static final int SORTORDER_INIT = SORTORDER_ASC;

    private final TreeViewer viewer;

    private final TreeColumn column;

    private int direction;

    private final String[] updateProperties;

    public ViewerSorter(TreeViewer viewer, TreeColumn column, String[] updateProperties) {
      this.viewer = viewer;
      this.column = column;
      this.direction = SORTORDER_INIT;
      this.updateProperties = updateProperties;

      this.column.addSelectionListener(new SelectionAdapter() {

        @Override
        public void widgetSelected(SelectionEvent e) {
          ViewerSorter thiz = ViewerSorter.this;
          if (thiz.viewer.getComparator() == thiz) {
            if (thiz.direction == SORTORDER_ASC) {
              thiz.becomeSorter(SORTORDER_DESC);
            } else if (thiz.direction == SORTORDER_DESC) {
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

      if (this.direction == SORTORDER_ASC) {
        this.column.getParent().setSortDirection(SWT.UP);
      } else {
        this.column.getParent().setSortDirection(SWT.DOWN);
      }

      if (this.viewer.getComparator() == this) {
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
      for (String sensibleProperty : this.updateProperties) {
        if (sensibleProperty.equals(property)) {
          return true;
        }
      }
      return false;
    }

  }
}
