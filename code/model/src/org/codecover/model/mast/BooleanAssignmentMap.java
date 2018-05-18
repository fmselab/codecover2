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

package org.codecover.model.mast;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A BooleanAssignmentMap is a map which assigns each BooleanAssignment a long
 * indicating the number of times this BooleanAssignment was evaluated.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: BooleanAssignmentMap.java 49 2009-06-01 08:52:19Z ahija $)
 */
public final class BooleanAssignmentMap
    implements Serializable {

    private static final long serialVersionUID = 8300108213347003689L;

    private final int length;

    private final Map<BooleanAssignment, Long> map;

    /**
     * Creates a {@code BooleanAssignmentMap}.
     *
     * @param length
     *            the length of the {@link BooleanAssignment}s.
     * @param data
     *            the coverage data
     */
    public BooleanAssignmentMap(int length, Map<BooleanAssignment, Long> data) {
        if (length < 0) {
            throw new IllegalArgumentException("length < 0");
        }

        if (data == null) {
            throw new NullPointerException("data == null");
        }

        this.length = length;

        if (data.size() == 0) {
            this.map = Collections.emptyMap();
        } else {
            final Map<BooleanAssignment, Long> dataCopy = new HashMap<BooleanAssignment, Long>();

            for (BooleanAssignment assignment : data.keySet()) {
                if (assignment.getLength() != length) {
                    throw new IllegalArgumentException(
                            "assignment.getLength() != length");
                }

                final Long value = data.get(assignment);
                if (value == null) {
                    throw new NullPointerException("value == null");
                }

                if (value < 0) {
                    throw new IllegalArgumentException("value < 0");
                }

                if (value == 0) {
                    throw new IllegalArgumentException("value == 0");
                } else {
                    dataCopy.put(assignment, value);
                }
            }

            this.map = Collections.unmodifiableMap(dataCopy);
        }
    }

    /*
     * This is a private version of the constructor which will not create a copy
     * of the Map and the lengths won't be checked. Therefore the caller has to
     * assure that the Map won't be changed and the lengths are correct.
     */
    private BooleanAssignmentMap(int length, Map<BooleanAssignment, Long> data,
            boolean unusedPrivateFlag) {
        this.length = length;

        if (data.size() == 0) {
            this.map = Collections.emptyMap();
        } else {
            this.map = data;
        }
    }

    private BooleanAssignmentMap(int length) {
        this.length = length;

        this.map = Collections.emptyMap();
    }

    /**
     * Creates an empty {@link BooleanAssignmentMap} with the given length.
     *
     * @param length
     *            the given length
     * @return the empty {@link BooleanAssignmentMap}.
     */
    public static BooleanAssignmentMap createEmptyMap(int length) {
        if (length < 0) {
            throw new IllegalArgumentException("length < 0");
        }

        return new BooleanAssignmentMap(length);
    }

    /**
     * @return the length of the {@link BooleanAssignment}s.
     */
    public int getLength() {
        return this.length;
    }

    /**
     * Gets the {@link Map} containing the data of this object
     *
     * @return the {@link Map} containing the data.
     */
    public Map<BooleanAssignment, Long> getData() {
        return this.map;
    }

    /**
     * Gets a set of all the {@link BooleanAssignment}s which were evaluated at
     * all.
     *
     * @return the set of {@link BooleanAssignment}s
     */
    public Set<BooleanAssignment> getEvaluatedAssignments() {
        return getData().keySet();
    }

    /**
     * Gets the number of times the {@link BooleanAssignment} was evaluated
     *
     * @param assignment
     *            the given assignment
     * @return the numebr of time the given {@link BooleanAssignment} was
     *         evaluated
     */
    public long get(BooleanAssignment assignment) {
        if (assignment == null) {
            throw new NullPointerException("assignment == null");
        }

        if (assignment.getLength() != getLength()) {
            throw new IllegalArgumentException(
                    "assignment.getLength() != getLength()");
        }

        final Long result = this.map.get(assignment);

        if (result == null) {
            return 0;
        } else {
            return result;
        }
    }

    /**
     * Merges a collection of {@link BooleanAssignmentMap}s which all have the
     * given length
     *
     * @param length
     *            the length of all {@link BooleanAssignmentMap}s
     * @param maps
     *            the collection of {@link BooleanAssignmentMap}s
     * @return a {@code BooleanAssignmentMap} containing the result of all given
     *         {@code BooleanAssignmentMap}s combined.
     */
    public static BooleanAssignmentMap merge(int length,
            Collection<BooleanAssignmentMap> maps) {
        if (length < 0) {
            throw new IllegalArgumentException("length < 0");
        }

        if (maps == null) {
            throw new NullPointerException("map == null");
        }

        switch (maps.size()) {
        case 0:
            return createEmptyMap(length);
        case 1: {
            final BooleanAssignmentMap map = maps.iterator().next();
            if (map.getLength() != length) {
                throw new IllegalArgumentException("map.getLength() != length");
            }
            return map;
        }
        default:
            final HashMap<BooleanAssignment, Long> newData = new HashMap<BooleanAssignment, Long>();
            for (BooleanAssignmentMap map : maps) {
                if (map.getLength() != length) {
                    throw new IllegalArgumentException(
                            "map.getLength() != length");
                }
                for (BooleanAssignment assignment : map
                        .getEvaluatedAssignments()) {
                    final long value = map.get(assignment);

                    final Long oldValueBoxed = newData.get(assignment);
                    final long oldValue;
                    if (oldValueBoxed == null) {
                        oldValue = 0;
                    } else {
                        oldValue = oldValueBoxed;
                    }

                    final long newValue = value + oldValue;

                    newData.put(assignment, newValue);
                }
            }
            // This constructor only may be called because we know no one else
            // has the newData map.
            return new BooleanAssignmentMap(length, newData, false);
        }
    }

    /**
     * Merges two {@link BooleanAssignmentMap}s
     *
     * @param map1
     *            the one map to be merged
     * @param map2
     *            the other map to be merged
     * @return a {@code BooleanAssignmentMap} containing the result of all given
     *         {@code BooleanAssignmentMap}s combined.
     */
    public static BooleanAssignmentMap merge(BooleanAssignmentMap map1,
            BooleanAssignmentMap map2) {
        if (map1 == null) {
            throw new NullPointerException("map1 == null");
        }

        if (map2 == null) {
            throw new NullPointerException("map2 == null");
        }

        if (map1.getLength() != map2.getLength()) {
            throw new IllegalArgumentException(
                    "map1.getLength() != map2.getLength()");
        }

        final ArrayList<BooleanAssignmentMap> maps = new ArrayList<BooleanAssignmentMap>();
        maps.add(map1);
        maps.add(map2);

        return merge(map1.getLength(), maps);
    }
}
