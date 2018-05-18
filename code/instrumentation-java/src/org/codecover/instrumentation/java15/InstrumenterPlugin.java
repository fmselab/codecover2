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

package org.codecover.instrumentation.java15;

import org.codecover.model.extensions.AbstractExtension;
import org.codecover.model.extensions.AbstractPlugin;
import org.codecover.model.extensions.Extension;

/**
 * The plugin class for the java instrumenter.
 * 
 * @author Steffen Kieß
 * 
 * @version 1.0 ($Id: InstrumenterPlugin.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterPlugin extends AbstractPlugin {
    /**
     * Constructor for a new {@link #InstrumenterPlugin()}, with the name
     * <code>Java 1.5 Instrumenter Plugin</code>.
     */
    public InstrumenterPlugin() {
        super("Java 1.5 Instrumenter Plugin", "", new Extension<?>[] {
                new AbstractExtension<org.codecover.instrumentation.InstrumenterDescriptor>(org.codecover.instrumentation.InstrumenterDescriptor.class, "org.codecover.instrumentation.java15.InstrumenterDescriptor") {
                    public org.codecover.instrumentation.InstrumenterDescriptor getObject() {
                        return new InstrumenterDescriptor();
                    }
                }
            });
    }
}
