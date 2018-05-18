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
import java.util.Collection;
import java.util.LinkedList;

import org.codecover.model.utils.file.DirectoryScanner;

/**
 * A {@link FileFoundListener}, that stores all {@link File}s found by 
 * {@link FileFoundListener#includedFileFound(File, String)} and 
 * {@link FileFoundListener#notIncludedFileFound(File, String)}.
 * 
 * @see DirectoryScanner#scan(File, FileFoundListener)
 * 
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: AllFileFoundListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AllFileFoundListener extends IncludedFileFoundListener {

    private LinkedList<File> notIncludedFiles = new LinkedList<File>();

    @Override
    public void notIncludedFileFound(File notIncludedFile, String relativePath) {
        this.notIncludedFiles.add(notIncludedFile);
    }

    /**
     * Returns all {@link File}s found by
     * {@link FileFoundListener#notIncludedFileFound(File, String)}.
     * 
     * @return All not included {@link File}s found.
     */
    public Collection<File> getNotIncludedFiles() {
        return this.notIncludedFiles;
    }

    @Override
    public boolean considerNotIncluded() {
        return true;
    }
}
