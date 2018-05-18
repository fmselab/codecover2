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
 * ChangeType is an enum used to describe the type of a change event: ADD is
 * used to describe that the object was added to its parent REMOVE is used to
 * describe that the object was removed from its parent CHANGE is used to
 * describe that some properties of the object were changed
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: ChangeType.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public enum ChangeType {
    /**
     * Describes that the object was added to its parent.
     */
    ADD,
    /**
     * Describes that the object was removed from its parent
     */
    REMOVE,
    /**
     * Describes that some properties of the object were changed
     */
    CHANGE
}
