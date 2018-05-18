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

package org.codecover.model.utils.file;

import java.io.File;
import java.nio.charset.Charset;

/**
 * This is a container for two {@link File}s.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: SourceTargetContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SourceTargetContainer {

    private final File target;

    private final File source;

    private final Charset charset;

    /**
     * Constructor for a new container.
     * 
     * @param source
     *            The source.
     * @param target
     *            The target.
     * 
     * @see #SourceTargetContainer(File, File, Charset)
     */
    public SourceTargetContainer(File source, File target) {
      this(source, target, null);
    }

    /**
     * Constructor for a new container.
     * 
     * @param source
     *            The source.
     * @param target
     *            The target.
     * @param charset 
     *            The charset of the file. (can be <code>null</code>)
     * 
     */
    public SourceTargetContainer(File source, File target, Charset charset) {
        this.source = source;
        this.target = target;
        this.charset = charset;
    }

    /**
     * @return the source
     */
    public File getSource() {
        return this.source;
    }

    /**
     * @return the target
     */
    public File getTarget() {
        return this.target;
    }

    /**
     * @return the charset of the file
     */
    public Charset getCharset() {
      return this.charset;
    }

    @Override
    public String toString() {
        return this.source.getAbsolutePath();
    }
}
