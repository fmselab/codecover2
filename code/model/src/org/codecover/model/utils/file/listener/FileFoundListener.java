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

package org.codecover.model.utils.file.listener;

import java.io.File;

import org.codecover.model.utils.file.DirectoryScanner;

/**
 * @see DirectoryScanner#scan(File, FileFoundListener)
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: FileFoundListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface FileFoundListener {
    /**
     * A {@link File}, that is included and not excluded has been found.
     * 
     * @param includedFile
     *            The found {@link File}.
     * @param relativePath
     *            The relative path of the file under the root directory.
     */
    public void includedFileFound(File includedFile, String relativePath);

    /**
     * A {@link File}, that not included or excluded has been found.
     * 
     * @param notIncludedFile
     *            The found {@link File}.
     * @param relativePath
     *            The relative path of the file under the root directory.
     */
    public void notIncludedFileFound(File notIncludedFile, String relativePath);

    /**
     * Do you want to grab not included files?
     * 
     * @return true &rarr; yes, inform me with
     *         {@link #notIncludedFileFound(File, String)}; false &rarr; I'm not
     *         interesed.
     */
    public boolean considerNotIncluded();
}
