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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.Stack;
import java.util.Vector;

/**
 * This is an implementation similar to a <b>stack</b>.<br>
 * <br>
 * In contrast to the Java {@link Stack}, this class uses a {@link LinkedList}
 * rather than a {@link Vector} to store the elements. This is more in the
 * common understanding of a stack.
 * <br>
 * We have called this class attic, that this collection is not mixed up with
 * the Java {@link Stack}. Moreover you can put elements on the top of a heap or
 * stack or you can push them up to the an attic.
 * <br>
 * For this reason, the first element added is known as the <b>top</b> and the
 * last element added is know as the {@link #bottom()}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: Attic.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @param <T>
 *            The generic parameter.
 */
public class Attic<T> implements Iterable<T> {
    private LinkedList<T> list;

    /**
     * Constructor, initializes the {@link Attic}.
     * 
     */
    public Attic() {
        this.list = new LinkedList<T>();
    }

    /**
     * Checks, if the {@link Attic} is empty
     * 
     * @return <code>true</code>, if the {@link Attic} contains no elements,
     *         <code>false</code> otherwise.
     */
    public boolean isEmpty() {
        return this.list.isEmpty();
    }

    /**
     * Gets the lowest element of the {@link Attic} without removing it.
     * 
     * @return the lowest element of the {@link Attic}
     */
    public T bottom() {
        return this.list.peek();
    }

    /**
     * Gets the lowest element of the {@link Attic} and removes it from the
     * {@link Attic}
     * 
     * @return the topmost element of the {@link Attic}
     */
    public T pop() {
        return this.list.poll();
    }

    /**
     * Adds the given element to the bottom of the {@link Attic}
     * 
     * @param element
     *            the given element
     */
    public void push(T element) {
        this.list.addFirst(element);
    }

    /**
     * Gets the number of elements in the {@link Attic}.
     * 
     * @return the number of elements
     */
    public int size() {
        return this.list.size();
    }

    /**
     * Calls {@link LinkedList#toString()}
     * 
     * @return the result of {@link LinkedList#toString()}
     * @see LinkedList#toString()
     */
    @Override
    public String toString() {
        return this.list.toString();
    }

    /**
     * Returns an {@link Iterator} of this Attic from the <b>bottom</b> to the
     * <b>top</b>.
     * 
     * @return An {@link Iterator}.
     */
    public Iterator<T> iterator() {
        return this.list.iterator();
    }
}
