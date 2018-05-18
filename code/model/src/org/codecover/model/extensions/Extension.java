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

package org.codecover.model.extensions;

import java.util.*;

import org.codecover.model.utils.*;

// might be Extension<Metric>, Extension<Criterion>, Extension<InstrumenterDescriptor> or Extension<ReportGenerator>

/**
 * An interface to be implemented for every extension made by a plugin.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Extension.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface Extension<T> {
    // normally the class name of the metric/etc.
    public String getName();
    
    public T getObject();
    
    public Class<T> getInterface();
}
