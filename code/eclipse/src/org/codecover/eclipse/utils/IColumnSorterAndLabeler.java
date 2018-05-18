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

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: IColumnSorterAndLabeler.java 55 2009-07-20 17:55:14Z ahija $)
 */
public interface IColumnSorterAndLabeler<T> extends ISorterAndLabeler<T> {

    public IColumnSorterAndLabeler<T> getNextSorter();

    public IColumnSorterAndLabeler<T> getDefaultSorter();
}
