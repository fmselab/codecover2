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

import java.util.Comparator;

import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeColumn;

/**
 * A {@link Comparator} that knows the sort direction and can get the label for an instance of the generic
 * type.
 *
 * @author Christoph Müller
 * @version 1.0 ($Id: InvertableComparator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ISorterAndLabeler<T> extends Comparator<T> {

    /** {@link TreeColumn#setText} */
    public String getColumnName();

    /** {@link Tree#setSortDirection(int)} */
    public int getSortDirection();

    /**
     * Returns the text for a value in a cell of the column.
     *
     * @see ILabelProvider#getText(Object)
     */
    public String getLabelText(T dataObject);

    /**
     * Returns the image for a value in a cell of the column
     *
     * @see ILabelProvider#getImage(Object)
     */
    public Image getColumnImage(T dataObject);
}
