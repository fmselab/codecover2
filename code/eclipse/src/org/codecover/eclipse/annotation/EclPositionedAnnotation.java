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

package org.codecover.eclipse.annotation;

import org.codecover.report.highlighting.annotation.Annotation;
import org.eclipse.jface.text.Position;

/**
 * Annotation object that includes its position.
 * 
 * @author  Johannes Langauf
 * @version 1.0 ($Id: EclPositionedAnnotation.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class EclPositionedAnnotation extends org.eclipse.jface.text.source.Annotation {

    private final Position position;
    
    /**
     * Create annotation with given position inside.
     * 
     * @param type
     *     the annotation type
     * @param position
     *     the position to embed
     */
    public EclPositionedAnnotation(String type, Position position) {
        super(type, false, null);
        this.position = position;
    }
    
    /**
     * Create Eclipse annotation at Position of an Annotation.
     * 
     * @param type
     *     the annotation type
     * @param position
     *     the annotation with the position information to extract
     */
    public EclPositionedAnnotation(String type, Annotation position) {
        super(type, false, null);
        this.position = new Position(position.getStartOffset(),
                position.getLength());
    }

    /**
     * Gets the position of this annotation
     * 
     * @return the {@link Position}
     */
    public Position getPosition() {
        return position;
    }

}