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
import org.codecover.model.mast.SourceFile;

/**
 * Mark a consecutive region of code. To be subclassed to add useful info.
 *
 * @author Johannes Langauf
 * @version 1.0 ($Id: DefaultAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DefaultAnnotation extends AbstractAnnotation implements FatAnnotation {
    
    private final Location location;
    
    /**
     * @param l
     */
    public DefaultAnnotation(Location l) {
        location = l;
    }

    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.IAnnotation#getLocation()
     */
    public Location getLocation() {
        return location;
    }
    
    @Override
    public int getEndOffset() {
        return getLocation().getEndOffset();
    }

    @Override
    public int getStartOffset() {
        return getLocation().getStartOffset();
    }
    
    /* (non-Javadoc)
     * @see org.codecover.report.highlighting.annotation.IAnnotation#getFile()
     */
    public SourceFile getFile() {
        return getLocation().getFile();
    }

    public String getContent() {
        return getContent(getFile().getContent());
    }
    
    @Override
    protected String getReference() {
        return "from '" + getFile().getFileName()
               + "' " + super.getReference();
    }
}
