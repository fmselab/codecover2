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
 * The generic interface ChangeListener<T> is to be implemented by classes
 * which want to listen for change events for objects of type T.
 * 
 * @param <T> the type of the objects
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: ChangeListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ChangeListener<T> {
    /**
     * This method is called each time a change occurs.
     *
     * Implementors of this method have to be aware that this
     * method will not be called in the GUI thread.
     *
     * @param changeType the type of the change
     * @param object the object affected by the change
     */
    public void changed(ChangeType changeType, T object);
}
