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
 * This is a {@link FileFoundListener}, that ignores all files found.
 * 
 * @see DirectoryScanner#scan(File, FileFoundListener)
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: DummyFileFoundListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class DummyFileFoundListener implements FileFoundListener {

    /** A {@link DummyFileFoundListener} instance for reusage. */
    public static final FileFoundListener INSTANCE = new DummyFileFoundListener();

    public void includedFileFound(File includedFile, String relativePath) {
        // ignore all files.
    }

    public void notIncludedFileFound(File notIncludedFile, String relativePath) {
        // ignore all files.
    }

    public boolean considerNotIncluded() {
        return false;
    }
}
