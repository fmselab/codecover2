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

package org.codecover.report.highlighting.annotation;

import org.codecover.model.mast.Location;

/**
 * Annotation to add named Anchor to reference region inside a code file.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultAnchorAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultAnchorAnnotation extends DefaultAnnotation implements AnchorAnnotation {

    //TODO: make final
    private String name;
    
    /**
     * @param locaton
     * the location to annotate
     * @param name
     * the name to reference this Anchor
     */
    public DefaultAnchorAnnotation (Location locaton, String name) {
        super(locaton);
        this.name = name;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.IAnchroAnnotation#getName()
     */
    public String getName() {
        return name;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.IAnchroAnnotation#setName(java.lang.String)
     */
    public void setName(String name) {
        this.name = name;
    }
}
