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
 * All methods in the data model which are used to register an event listener
 * return an instance implementing ListenerHandle. When dispose() ist called on
 * this instance, the event listener will be removed.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: ListenerHandle.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface ListenerHandle extends Disposable {
    // No methods needed.
}
