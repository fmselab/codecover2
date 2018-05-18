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

package org.codecover.instrumentation.booleanterms;

import java.io.IOException;
import java.io.Writer;
import java.util.List;
import java.util.Vector;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.BooleanTerm;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;

/**
 * This class extends the {@link InstrBooleanTerm} and represents a Basic
 * Boolean Term.<br>
 * <br>
 * A {@link InstrBasicBooleanTerm} is meant to be the leaf of the boolean term
 * tree, that can not be subdivided into smaller {@link InstrBasicBooleanTerm}s.<br>
 * The method {@link #termToString()} just returns the image of the element of
 * the syntaxtree. The method {@link #getAllBasicBooleanTerms(List)} adds this
 * object to the Queue.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrBasicBooleanTerm.java 1 2007-12-12 17:37:26Z t-scheller $)
 * @see InstrBooleanTerm
 */
public class InstrBasicBooleanTerm extends InstrBooleanTerm {
    private String image;

    private int startOffset;

    private int endOffset;

    private boolean containsAssignments;
    
    /**
     * Creates a new {@link InstrBasicBooleanTerm} from the given image.
     * 
     * @param image
     *            The image of the {@link InstrBasicBooleanTerm}&mdash;e.g.<br> "<code>vector.size() > 10</code>".
     * @param startOffset
     *            The startOffset of the image of this term for {@link Location}
     *            creation.
     * @param endOffset
     *            The endOffsett of the image of this term for {@link Location}
     *            creation.
     * 
     * @throws NullPointerException
     *             if the image is <code>null</code>
     */
    public InstrBasicBooleanTerm(String image, int startOffset, int endOffset) {
        if (image == null) {
            throw new NullPointerException("image == null");
        }
        this.image = image;
        this.startOffset = startOffset;
        this.endOffset = endOffset;
        this.containsAssignments = false;
    }

    /**
     * This method sets the {@link #image} of the {@link InstrBasicBooleanTerm}.
     * 
     * @param newImage
     *            The new image of the basic boolean term.
     */
    public void modifyImage(String newImage) {
        this.image = newImage;
    }

    /**
     * Does this {@link InstrBasicBooleanTerm} contain an assignment like
     * <code>=, |=, &=, ^=</code>.
     * 
     * @return true &rarr; yes; false &rarr; no
     */
    public boolean containsAssignments() {
        return this.containsAssignments;
    }

    /**
     * Does this {@link InstrBasicBooleanTerm} contain an assignment like
     * <code>=, |=, &=, ^=</code>.
     * 
     * @param containsAssignments true &rarr; yes; false &rarr; no
     */
    public void setContainsAssignments(boolean containsAssignments) {
        this.containsAssignments = containsAssignments;
    }

    @Override
    public String termToString() {
        return this.image;
    }

    @Override
    public void writeToTarget(Writer target) throws IOException {
        target.write(this.image);
    }

    @Override
    public void getAllBasicBooleanTerms(List<InstrBasicBooleanTerm> termQueue) {
        termQueue.add(this);
    }

    @Override
    public BooleanTerm toBooleanTerm(MASTBuilder database,
            SourceFile sourceFile) {
        List<Location> locationList = new Vector<Location>(1);
        if (this.startOffset > -1 && this.endOffset > -1) {
            locationList.add(database.createLocation(sourceFile,
                    this.startOffset,
                    this.endOffset));
        }
        return database.createBasicBooleanTerm(database.createLocationList(locationList));
    }

    @Override
    public void access(InstrBooleanVisitor visitor) {
        visitor.visit(this);
    }
}
