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

package org.codecover.instrumentation;

import java.io.File;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.codecover.model.TestSessionContainer;

/**
 * A directive to set the UUID, the instrumenter should use for
 * {@link TestSessionContainer#TestSessionContainer(org.codecover.model.mast.HierarchyLevel, org.codecover.model.utils.Logger, java.util.List, Set, String, java.util.Date)}.
 * 
 * @see Instrumenter#instrument(File, File, java.util.Collection,
 *      org.codecover.model.MASTBuilder, Map)
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: UUIDDirective.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class UUIDDirective extends InstrumenterDirective {
    /** The static instance for reuse. */
    public static final UUIDDirective INSTANCE = new UUIDDirective();

    /** The name of the key of this directive */
    public static final String KEY = "UUID";

    private static final String description = "The UUID, the instrumenter "
            + "should use for the creation of a TestSessionContainer";

    /**
     * Constructor for a new UUID directive.
     */
    private UUIDDirective() {
        super(KEY, description);
    }

    @Override
    public String parseValue(String value) {
        return value;
    }

    @Override
    public String getDefaultValue() {
        return UUID.randomUUID().toString();
    }
}