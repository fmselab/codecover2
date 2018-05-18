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

package org.codecover.ant;

import java.util.Set;
import java.util.TreeSet;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.DataType;
import org.codecover.model.utils.CollectionUtil;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CriteriaList.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CriteriaList extends DataType {
    private final Set<String> criteria = new TreeSet<String>();

    /**
     * Adds a configured {@link CriteriaListItem} to this {@link CriteriaList}.
     * 
     * @param criterion
     *                the {@link CriteriaListItem} to add.
     */
    public void addConfiguredCriterion(CriteriaListItem criterion) {
        final String name = criterion.getName();
        if (this.criteria.contains(name)) {
            throw new BuildException("The criterion '" + name
                    + "' appears twice.");
        }
        this.criteria.add(name);
    }

    /**
     * Gets the {@link Set} of criteria.
     * 
     * @return the {@link Set} of criteria.
     */
    public Set<String> getCriteria() {
        return CollectionUtil.copy(this.criteria);
    }
}
