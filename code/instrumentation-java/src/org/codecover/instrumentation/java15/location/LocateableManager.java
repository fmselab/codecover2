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
package org.codecover.instrumentation.java15.location;

import java.util.LinkedList;

import org.codecover.instrumentation.java15.visitor.InstrumentationVisitor;
import org.codecover.instrumentation.java15.visitor.TreeDumperWithException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.mast.Statement;

/**
 * This class is a manager for levels. These levels refer to {@link Location}
 * starts and ends of an element in the {@link InstrumentationVisitor}&mdash;e.g.
 * a {@link Statement}.<br>
 * These levels are managed by a kind of stack. With {@link #pushNewLevel(boolean)},
 * you can push a new Location to the stack. With {@link #popLevel()}, re receive
 * the topmost.<br>
 * When pushing, you can decide, whether the level should be explicit or 
 * implicit. Explicit means, the the {@link Location} information of the
 * explicit level are sliced out of the location level below it.<br>
 * Using the default methods, the {@link LocateableManager} will use the next
 * start offset of the given {@link TreeDumperWithException} for the start
 * offset of a pushed level and the {@link TreeDumperWithException#getLastEndOffset()}
 * as the end offset. If you need other values, you can use {@link #pushNewLevel(int, boolean)}
 * respectively {@link #popLevel(int)}.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: LocateableManager.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LocateableManager {
    private final TreeDumperWithException treeDumper;
    private final SourceFile sourceFile;
    private final MASTBuilder builder;
    private LinkedList<LocateableLevel> levels;

    /**
     * The constructor for a new {@link LocateableManager}.
     * 
     * @param treeDumper
     *                The {@link TreeDumperWithException} used for Location
     *                information like
     *                {@link TreeDumperWithException#addOffsetListener(OffsetListener)}
     *                and {@link TreeDumperWithException#getLastEndOffset()}.
     * @param sourceFile
     *                The {@link SourceFile}, all created {@link Location}s
     *                are made from.
     * @param builder
     *                The {@link MASTBuilder} to create {@link Location} and
     *                {@link LocationList}.
     */
    public LocateableManager(TreeDumperWithException treeDumper,
                             SourceFile sourceFile,
                             MASTBuilder builder) {
        this.treeDumper = treeDumper;
        this.sourceFile = sourceFile;
        this.builder = builder;
        this.levels = new LinkedList<LocateableLevel>();
    }

    /**
     * Pushes a new level.
     * 
     * @param startOffset
     *                The start offset to use.
     * @param explicit
     *                true &rarr; slices out {@link Location} information from
     *                the level below the pushed one.
     *                
     * @see #pushNewLevel(boolean)
     */
    public void pushNewLevel(int startOffset, boolean explicit) {
        this.levels.addLast(new LocateableLevel(explicit));
        this.levels.getLast().startOffset(startOffset);
    }

    /**
     * Pushes a new level.<br>
     * <br>
     * {@link TreeDumperWithException#addOffsetListener(OffsetListener)} is used
     * to get the start offset.
     * 
     * @param explicit
     *                true &rarr; slices out {@link Location} information from
     *                the level below the pushed one.
     * 
     * @see #pushNewLevel(int, boolean)
     */
    public void pushNewLevel(boolean explicit) {
        this.levels.addLast(new LocateableLevel(explicit));
        this.treeDumper.addOffsetListener(this.levels.getLast());
    }

    /**
     * Pops the highest level.
     * 
     * @param endOffset
     *                The end offset used for this level.
     * 
     * @return The created {@link LocationList} honoring start offset, end
     *         offset and explicit child levels.
     *
     * @see #popLevel()
     */
    public LocationList popLevel(int endOffset) {
        LocateableLevel topLevel = this.levels.removeLast();
        LocationList itsLocations = topLevel.toLocationList(endOffset,
                this.sourceFile,
                this.builder);
        if (topLevel.isExplicit() && !this.levels.isEmpty()) {
            this.levels.getLast().sliceOut(itsLocations);
        }
        return itsLocations;
    }

    /**
     * Pops the highest level.<br>
     * <br>
     * {@link TreeDumperWithException#getLastEndOffset()} is used as the end
     * offset.
     * 
     * @return The created {@link LocationList} honoring start offset, end
     *         offset and explicit child levels.
     *
     * @see #popLevel(int)
     */
    public LocationList popLevel() {
        return popLevel(this.treeDumper.getLastEndOffset());
    }

    /**
     * Are no levels on the stack?
     * 
     * @return true &rarr; yes; false &rarr; no
     */
    public boolean isEmpty() {
        return this.levels.isEmpty();
    }
}
