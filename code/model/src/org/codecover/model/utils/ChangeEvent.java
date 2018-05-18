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
 * This type provides an implementation for an event. It has a method which
 * should be called for each listener which is registered and a method which
 * should be called when the event is raised. This type safe for mutlithreaded
 * operation.
 * 
 * @param <T>
 *            the type of the objects
 * @author Steffen Kieß
 * @version 1.0 ($Id: ChangeEvent.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ChangeEvent<T> {
    protected Object lock = new Object();

    private long actualId = 0;

    protected Map<Long, ChangeListener<T>> listeners = new TreeMap<Long, ChangeListener<T>>();

    /**
     * Registers the given listener and returns an instance of
     * {@link ListenerHandle} with which the listener can be unregistered
     * 
     * @param listener
     *            the given listener
     * @return the created {@link ListenerHandle}
     */
    public ListenerHandle addListener(ChangeListener<T> listener) {
        if (listener == null) {
            throw new NullPointerException("listener == null");
        }

        final long id;

        synchronized (this.lock) {
            id = ++this.actualId;
            this.listeners.put(id, listener);
        }

        return new ListenerHandle() {
            public void dispose() {
                synchronized (ChangeEvent.this.lock) {
                    ChangeEvent.this.listeners.remove(id);
                }
            }
        };
    }

    /**
     * This method should be called each time a change occurs.
     * 
     * @param changeType
     *            the type of the change
     * @param object
     *            the object affected by the change
     */
    public void emitChanged(ChangeType changeType, T object) {
        final List<ChangeListener<T>> listenersCopy;

        synchronized (this.lock) {
            listenersCopy = new Vector<ChangeListener<T>>(this.listeners
                    .values());
        }

        for (ChangeListener<T> listener : listenersCopy) {
            listener.changed(changeType, object);
        }
    }
}
