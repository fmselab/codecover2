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

package org.codecover.ant;

import org.apache.tools.ant.types.DataType;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: Command.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class Command extends DataType {
    /**
     * Executes the {@link Command} in the given context.
     * 
     * @param context
     *                the given {@link Context}.
     */
    public abstract void run(Context context);
}
