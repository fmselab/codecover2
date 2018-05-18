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

/**
 * An immutable pair of non <code>null</code> values.
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: Pair.java 64 2009-09-28 15:11:11Z ahija $)
 * @param <S>
 *            the type of the first element.
 * @param <T>
 *            the type of the second element.
 */
public final class Pair<S, T> {
    /**
     * The first element of this {@link Pair}
     */
    public final S first;

    /**
     * The second element of this {@link Pair}
     */
    public final T second;

    /**
     * Creates a {@link Pair} with the given elements.
     *
     * @param first
     *            the first element of the pair.
     * @param second
     *            the second element of the pair.
     */
    public Pair(S first, T second) {
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean equals(Object o) {
        if (o == null || !(o instanceof Pair<?, ?>)) {
            return false;
        }

        final Pair<?, ?> other = (Pair<?, ?>) o;
        return this.first.equals(other.first)
                && this.second.equals(other.second);
    }

    @Override
    public int hashCode() {
        return 17 * this.first.hashCode() + 31 * this.second.hashCode();
    }

    /**
     * Creates a new pair with the given elements and element types.
     *
     * @param <S>
     *            the type of the first element
     * @param <T>
     *            the type of the second element
     * @param first
     *            the first element
     * @param second
     *            the second element.
     * @return the created instance of {@link Pair}.
     */
    public static <S, T> Pair<S, T> create(S first, T second) {
        return new Pair<S, T>(first, second);
    }
}
