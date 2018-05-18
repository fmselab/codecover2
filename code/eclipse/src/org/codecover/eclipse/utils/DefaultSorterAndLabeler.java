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

import java.util.Arrays;
import java.util.Collection;

import org.eclipse.swt.graphics.Image;

/**
 * A default implementation of {@link IColumnSorterAndLabeler}.
 *
 * @author Markus Wittlinger
 * @version 1.0 ($Id: AbstractInvertableComparator.java 1697 2007-07-24
 *          16:58:09Z wittlims $)
 * @param <T>
 */
public class DefaultSorterAndLabeler<T> implements
        IColumnSorterAndLabeler<T> {
    private IColumnSorterAndLabeler<T> defaultSorter = null;

    private IColumnSorterAndLabeler<T> nextSorter = null;

    private ISorterAndLabeler<T> sorterAndLabeler = null;

    public static <T> IColumnSorterAndLabeler<T> constructComparator(
            ISorterAndLabeler<T> ... labelProviderAndDirection) {
        return constructComparator(Arrays.asList(labelProviderAndDirection));
    }

    public static <T> IColumnSorterAndLabeler<T> constructComparator(
            Collection<ISorterAndLabeler<T>> labelProviderAndDirection) {
        // contains the first sorterAndLabeler
        DefaultSorterAndLabeler<T> firstSortAndLabel = null;
        // contains a backreference to the previos SorterAndLabel in order to set the ".nextSorter"
        DefaultSorterAndLabeler<T> previousSortAndLabel = null;

        for (ISorterAndLabeler<T> thisSorterAndLabeler : labelProviderAndDirection)
        {
            DefaultSorterAndLabeler<T> thisDefaultSorterAndLabeler = new DefaultSorterAndLabeler<T>();
            thisDefaultSorterAndLabeler.sorterAndLabeler = thisSorterAndLabeler;

            if (firstSortAndLabel == null)
            {
                firstSortAndLabel = thisDefaultSorterAndLabeler;
            }
            thisDefaultSorterAndLabeler.defaultSorter = firstSortAndLabel;

            if (previousSortAndLabel != null)
            {
                previousSortAndLabel.nextSorter = thisDefaultSorterAndLabeler;
            }
            previousSortAndLabel = thisDefaultSorterAndLabeler;
        }

        // after the last comes the first
        previousSortAndLabel.nextSorter = firstSortAndLabel;

        return firstSortAndLabel;
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public IColumnSorterAndLabeler<T> getNextSorter() {
        return this.nextSorter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public IColumnSorterAndLabeler<T> getDefaultSorter() {
        return this.defaultSorter;
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public int compare(T o1, T o2) {
        return this.sorterAndLabeler.compare(o1, o2);
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public String getLabelText(T dataObject) {
        return this.sorterAndLabeler.getLabelText(dataObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public Image getColumnImage(T dataObject) {
        return this.sorterAndLabeler.getColumnImage(dataObject);
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public String getColumnName() {
        return this.sorterAndLabeler.getColumnName();
    }

    /**
     * {@inheritDoc}
     */
    @Override
	public int getSortDirection() {
        return this.sorterAndLabeler.getSortDirection();
    }
}
