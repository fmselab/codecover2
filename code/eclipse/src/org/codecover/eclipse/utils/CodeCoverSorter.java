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

package org.codecover.eclipse.utils;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.TreeColumn;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: CodeCoverSorter.java 55 2009-07-20 17:55:14Z ahija $)
 */
public class CodeCoverSorter extends ViewerSorter {

    /**
     * The key to be used in storing the comparator in the column
     */
    public static final String COMPARATOR_KEY = "org.codecover.eclipse.utils.InvertableComparator"; //$NON-NLS-1$

    private TreeViewer viewer;

    private TreeColumn[] columns;

    /**
     * Constructor
     *
     * @param columns   the columns in the order they are displayed (must have
     *                  the correct indices)
     */
    public CodeCoverSorter(TreeViewer viewer, TreeColumn[] columns) {
        this.viewer = viewer;
        this.columns = columns;

        for (TreeColumn treeColumn : columns) {
            createSelectionListener(treeColumn);
        }
    }

    /**
     * (non-Javadoc)
     *
     * @see org.eclipse.jface.viewers.ViewerComparator#compare(Viewer, Object, Object)
     */
    @Override
    public int compare(Viewer viewer, Object o1, Object o2) {
        Object d1 = o1;
        Object d2 = o2;
        for (int i = 0; i < this.columns.length; i++) {
            IColumnSorterAndLabeler<? super Object> comparator = getSorterAndLabeler(this.columns[i]);
            int result = comparator.compare(d1, d2);
            if (result != 0) {
                return result;
            }
        }
        return 0;
    }

    private void createSelectionListener(final TreeColumn column) {
        column.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                sortUsing(column);
            }
        });
    }

    private void sortUsing(TreeColumn column) {
        Object[] expandedElements;
        if (column == this.columns[0]) {
            setSorterAndLabeler(column, getSorterAndLabeler(column).getNextSorter());
        } else {
            for (int i = 0; i < this.columns.length; i++) {
                if (column == this.columns[i]) {
                    System.arraycopy(this.columns, 0, this.columns, 1, i);
                    this.columns[0] = column;
                    setSorterAndLabeler(column, getSorterAndLabeler(column).getDefaultSorter());
                    break;
                }
            }
        }

        IColumnSorterAndLabeler<? super Object> sorterAndLabeler = getSorterAndLabeler(column);
        this.viewer.getTree().setSortColumn(column);
        this.viewer.getTree().setSortDirection(sorterAndLabeler.getSortDirection());
        column.setText(sorterAndLabeler.getColumnName());

        expandedElements = this.viewer.getExpandedElements();
        this.viewer.refresh();
        this.viewer.setExpandedElements(expandedElements);
    }

    @SuppressWarnings("unchecked")
    private IColumnSorterAndLabeler<? super Object> getSorterAndLabeler(
            TreeColumn column) {
        return (IColumnSorterAndLabeler<? super Object>) column.getData(COMPARATOR_KEY);
    }

    private void setSorterAndLabeler(TreeColumn column,
            IColumnSorterAndLabeler<? super Object> sorterAndLabeler) {
        column.setData(COMPARATOR_KEY, sorterAndLabeler);
    }
}
