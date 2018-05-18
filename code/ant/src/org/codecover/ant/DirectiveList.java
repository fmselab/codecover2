/******************************************************************************
 * Copyright (c) 2008 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/
package org.codecover.ant;

import java.util.ArrayList;
import java.util.List;

import org.apache.tools.ant.types.DataType;
import org.codecover.model.utils.CollectionUtil;

/**
 * An Extension for {@link InstrumentCommand} to represent Directives.
 *
 * @author Christoph Müller
 *
 * @version 1.0 ($Id$)
 */
public class DirectiveList extends DataType {

    private final List<Directive> directives = new ArrayList<Directive>();

    public void addConfiguredDirective(Directive directive) {
        this.directives.add(directive);
    }

    public List<Directive> getDirectives() {
        return CollectionUtil.copy(this.directives);
    }
}
