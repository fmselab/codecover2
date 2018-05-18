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

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;

/**
 * An {@link ISorterAndLabeler} that inverses another instance.
 *
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InvertableComparator.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InverseSorterAndLabeler<T> implements ISorterAndLabeler<T> {

    private final ISorterAndLabeler<T> otherInstance;

    public InverseSorterAndLabeler(ISorterAndLabeler<T> otherInstance) {
        this.otherInstance = otherInstance;
    }

    @Override
	public int compare(T o1, T o2) {
        // just the inversed result
        return -1 * this.otherInstance.compare(o1, o2);
    }

    @Override
	public String getLabelText(T dataObject) {
        return this.otherInstance.getLabelText(dataObject);
    }

    @Override
	public Image getColumnImage(T dataObject) {
        return this.otherInstance.getColumnImage(dataObject);
    }

    @Override
	public String getColumnName() {
        return this.otherInstance.getColumnName();
    }

    @Override
	public int getSortDirection() {
        if (this.otherInstance.getSortDirection() == SWT.UP) {
            return SWT.DOWN;
        }
        return SWT.UP;
    }
}
