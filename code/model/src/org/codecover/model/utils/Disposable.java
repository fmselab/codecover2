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
 * The Disposable interface is implemented by all classes which contain
 * resources which have to be freed once the class isn't needed anymore. For
 * this purpose its only method dispose() should be called.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Disposable.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Disposable {
    /**
     * Releases the resources allocated by this object.
     * <p>
     * If this method is called multiple times, the later invocations are
     * ignored.
     */
    public void dispose();
}
