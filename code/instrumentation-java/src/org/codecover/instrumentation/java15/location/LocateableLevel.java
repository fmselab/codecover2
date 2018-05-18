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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Set;
import java.util.TreeSet;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.LocationList;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.Logger;

/**
 * This is a class to manage a location level of the {@link LocateableManager}.
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: LocateableLevel.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see LocateableManager For description of the level handling.
 */
class LocateableLevel implements OffsetListener {
    private final boolean explicit;
    private final Set<Location> slicedOut;
    private int startOffset;
    /**
     * Creates a new {@link LocateableLevel}.
     *
     * @param explicit A flag to remember, when asked for {@link #isExplicit()}.
     */
    LocateableLevel(boolean explicit) {
        this.explicit = explicit;
        this.slicedOut = new TreeSet<Location>(LocationComparator.INSTANCE);
        this.startOffset = -1;
    }

    /**
     * Is this level explicit?
     * 
     * @return true &rarr; yes; false &rarr; no
     */
    public boolean isExplicit() {
        return this.explicit;
    }

    /**
     * Remembers the {@link Location}s of this {@link LocationList} and slices
     * out their location ranges from the location information of this level.
     * 
     * @param locationList The {@link LocationList} to use for slicing out.
     */
    public void sliceOut(LocationList locationList) {
        this.slicedOut.addAll(locationList.getLocations());
    }

    /**
     * Creates a {@link LocationList} honoring start offset, end offset and
     * explicit child levels.<br>
     * <br>
     * The ranges of all {@link Location}s added by
     * {@link #sliceOut(LocationList)} are deleted out of the {@link Location}
     * of this level.<br>
     * {@link Logger#fatal(String)} is used to handle illegal states.
     * 
     * @param endOffset
     *                The end offset of this level.
     * @param sourceFile
     *                The {@link SourceFile}, all created {@link Location}s
     *                are made from.
     * @param builder
     *                The {@link MASTBuilder} to create {@link Location} and
     *                {@link LocationList}.
     * @return The created {@link LocationList}.
     */
    public LocationList toLocationList(int endOffset,
                                       SourceFile sourceFile,
                                       MASTBuilder builder) {
        Logger logger = builder.getLogger();
        if ((this.startOffset == -1 && endOffset == -1) ||
            (this.startOffset == endOffset)) {
            return builder.createEmptyLocationList();
        } else if (this.startOffset > endOffset) {
            logger.fatal("this.startOffset > this.endOffset");
            return null;
        } else if (this.startOffset != -1 && endOffset != -1) {
            List<Location> locationList;
            if (this.slicedOut.isEmpty()) {
                // there are no Locations to slice out
                Location singleLocation = builder.createLocation(sourceFile, this.startOffset, endOffset);
                locationList = Collections.<Location>singletonList(singleLocation);
            } else {
                List<StartEnd> startEndList = new LinkedList<StartEnd>();
                startEndList.add(new StartEnd(this.startOffset, endOffset));
                for (Location sliceOut : this.slicedOut) {
                    if (sliceOut.getFile() != sourceFile) {
                        throw new IllegalStateException("sliceOut.getFile() != sourceFile");
                    }
                    int sliceStart = sliceOut.getStartOffset();
                    int sliceEnd = sliceOut.getEndOffset();
                    ListIterator<StartEnd> startEndIterator = startEndList.listIterator();
                    START_END : while (startEndIterator.hasNext()) {
                        StartEnd thisStartEnd = startEndIterator.next();
                        // There are different variants, how thisStartEnd and
                        // thisSliceOut lie

                        //      |tSE|
                        // |sO|       |sO|
                        if (sliceEnd <= thisStartEnd.start ||
                            thisStartEnd.end <= sliceStart) {
                            continue START_END;
                        }
                        //  | tSE |    | tSE |   | tSE |     | tSE |
                        //  | SO  |   | SO   |   |  SO  |   |   SO  |
                        if (thisStartEnd.start >= sliceStart &&
                            sliceEnd >= thisStartEnd.end) {
                            startEndIterator.remove();
                            continue START_END;
                        }
                        //  | tSE |   | tSE |
                        //  | SO |   | SO |
                        if (thisStartEnd.start >= sliceStart &&
                            sliceEnd < thisStartEnd.end) {
                            thisStartEnd.start = sliceEnd;
                            continue START_END;
                        }
                        //  | tSE |   | tSE |
                        //   | SO |     | SO |
                        if (thisStartEnd.start < sliceStart &&
                            sliceEnd >= thisStartEnd.end) {
                            thisStartEnd.end = sliceStart;
                            continue START_END;
                        }
                        //  | tSE |
                        //   |SO|
                        if (thisStartEnd.start < sliceStart &&
                            sliceEnd < thisStartEnd.end) {
                            // first create new with old .end
                            startEndIterator.add(new StartEnd(sliceEnd, thisStartEnd.end));
                            // now update .end
                            thisStartEnd.end = sliceStart;
                            continue START_END;
                        }

                        throw new IllegalStateException("unknown state: " +
                                "startEnd: " + thisStartEnd.start + ".." + thisStartEnd.end +
                                " slice: " + sliceStart + ".." + sliceEnd);
                    }
                } // for sliceOut

                // startEndList contains now sliced parts
                locationList = new ArrayList<Location>(startEndList.size());
                for (StartEnd thisStartEnd : startEndList) {
                    if (thisStartEnd.start == thisStartEnd.end) {
                        continue;
                    }
                    if (thisStartEnd.start > thisStartEnd.end) {
                        logger.fatal("thisStartEnd.start > thisStartEnd.end");
                        return null;
                    }
                    locationList.add(builder.createLocation(sourceFile,
                            thisStartEnd.start, thisStartEnd.end));
                }
            }
            return builder.createLocationList(locationList);
        } else {
            logger.fatal("startOffset != -1 ^ endOffset != -1");
            return null;
        }
    }

    /**
     * Sets the start offset. This should be <code>>= 0</code>.
     *
     * @param itsStartOffset The start offset of this level.
     */
    public void startOffset(int itsStartOffset) {
        if (itsStartOffset < 0) {
            throw new IllegalArgumentException("startOffset < 0");
        }
        this.startOffset = itsStartOffset;
    }

    /**
     * A simple {@link Comparator} to sort {@link Location}s by start and end
     * offset.
     * 
     * @author Christoph Müller 
     *
     * @version 1.0 ($Id: LocateableLevel.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    private static class LocationComparator implements Comparator<Location> {
        static final Comparator<Location> INSTANCE = new LocationComparator();
        
        public int compare(Location o1, Location o2) {
            int returnValue;

            returnValue = o1.getStartOffset() - o2.getStartOffset();
            if (returnValue == 0) {
                returnValue = o1.getEndOffset() - o2.getEndOffset();
            }

            return returnValue;
        }
    }

    /**
     * A container for two integers: <code>start</code> and <code>end</code>.
     * 
     * @author Christoph Müller 
     *
     * @version 1.0 ($Id: LocateableLevel.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    private static class StartEnd {
        int start;
        int end;

        StartEnd(int start, int end) {
            this.start = start;
            this.end = end;
        }
    }
}
