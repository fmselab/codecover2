/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.utils;

import java.util.*;

/**
 * This class contains utility functions for creating immutable copies of
 * collections.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CollectionUtil.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CollectionUtil {
    private CollectionUtil() {
        // Do nothing.
    }

    /**
     * Copies the given {@link Set} into a new {@link Set}.<br>
     * 
     * 
     * @param <T>
     *            the type of the entries of the set
     * @param data
     *            the given set
     * @return If the given set was empty, a {@link Collections#emptySet()} is
     *         returned<br>
     *         If the given set contained only one element, a
     *         {@link Collections#singleton(Object)}, containing said element,
     *         is returned <br>
     *         If the given set contained more than one element, a
     *         {@link Collections#unmodifiableSet(Set)}, containing the
     *         elements of the given set, is returned.
     */
    public static <T> Set<T> copy(Set<T> data) {
        final int size = data.size();

        switch (size) {
        case 0:
            return Collections.emptySet();
        case 1:
            return Collections.singleton(data.iterator().next());
        default:
            return Collections.unmodifiableSet(new HashSet<T>(data));
        }
    }

    /**
     * Copies the given {@link List} into a new {@link List}.<br>
     * 
     * 
     * @param <T>
     *            the type of the entries of the list
     * @param data
     *            the given list
     * @return If the given list was empty, a {@link Collections#emptyList()} is
     *         returned<br>
     *         If the given list contained only one element, a
     *         {@link Collections#singletonList(Object)}, containing said
     *         element, is returned <br>
     *         If the given list contained more than one element, a
     *         {@link Collections#unmodifiableList(List)}, containing the
     *         elements of the given list, is returned.
     */
    public static <T> List<T> copy(List<T> data) {
        final int size = data.size();

        switch (size) {
        case 0:
            return Collections.emptyList();
        case 1:
            return Collections.singletonList(data.get(0));
        default:
            return Collections.unmodifiableList(new ArrayList<T>(data));
        }
    }

    /**
     * Copies the given {@link Map} into a new {@link Map}.<br>
     * 
     * @param <A>
     *            the type of the keys of the map
     * @param <B>
     *            the type of the values of the map
     * @param data
     *            the given map
     * @return If the given map was empty, a {@link Collections#emptyMap()} is
     *         returned<br>
     *         If the given map contained only one entry, a
     *         {@link Collections#singletonMap(Object, Object)}, containing
     *         said entry, is returned <br>
     *         If the given map contained more than one element, a
     *         {@link Collections#unmodifiableMap(Map)}, containing the entries
     *         of the given map, is returned.
     */
    public static <A, B> Map<A, B> copy(Map<A, B> data) {
        final int size = data.size();

        switch (size) {
        case 0:
            return Collections.emptyMap();
        case 1:
            final A key = data.keySet().iterator().next();
            return Collections.singletonMap(key, data.get(key));
        default:
            return Collections.unmodifiableMap(new HashMap<A, B>(data));
        }
    }

    /**
     * Copies the given {@link Map} containing another {@link Map} into a new
     * {@link Map}.
     * 
     * @param <A>
     *            the type of the keys of the outer map
     * @param <B>
     *            the type of the keys of the map, that is the value of the
     *            outer map
     * @param <C>
     *            the type of the values of the map, that is the value of the
     *            outer map
     * @param data
     *            the given map
     * @return If the given map was empty, a {@link Collections#emptyMap()} is
     *         returned<br>
     *         If the given map contained only one entry, a
     *         {@link Collections#singletonMap(Object, Object)}, containing
     *         said entry, is returned <br>
     *         If the given map contained more than one element, a
     *         {@link Collections#unmodifiableMap(Map)}, containing the entries
     *         of the given map, is returned.
     */
    public static <A, B, C> Map<A, Map<B, C>> copyDeep(Map<A, Map<B, C>> data) {
        final int size = data.size();

        switch (size) {
        case 0:
            return Collections.emptyMap();
        case 1:
            final A key = data.keySet().iterator().next();
            return Collections.singletonMap(key, copy(data.get(key)));
        default:
            final Map<A, Map<B, C>> newData = new HashMap<A, Map<B, C>>();
            for (Map.Entry<A, Map<B, C>> entry : data.entrySet()) {
                newData.put(entry.getKey(), copy(entry.getValue()));
            }
            return Collections.unmodifiableMap(newData);
        }
    }
}
