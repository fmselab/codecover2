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

import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * This is a {@link FileFoundListener}, which grabs included and excluded
 * files.<br>
 * <br>
 * These files are resolved to a given <code>source folder</code> and a given
 * <code>target folder</code>. Finally this pair of files&mdash;source file
 * and target file&mdash;are packed to a {@link SourceTargetContainer} and added
 * to a {@link Collection}. These collection get be go by
 * {@link #getIncludedSourceTargetContainers()} and
 * {@link #getNotIncludedSourceTargetContainers()}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: SourceTargetAllListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SourceTargetAllListener extends SourceTargetIncludedListener {

    private Collection<SourceTargetContainer> notIncludedContainer;

    /**
     * Constructor for a new {@link SourceTargetAllListener}.
     * 
     * @param sourceFolder
     *            The source folder for all included files.
     * @param targetFolder
     *            The target folder for all included files.
     */
    public SourceTargetAllListener(File sourceFolder, File targetFolder) {
        super(sourceFolder, targetFolder);
        this.notIncludedContainer = new LinkedList<SourceTargetContainer>();
    }

    @Override
    public Collection<SourceTargetContainer> getNotIncludedSourceTargetContainers() {
        return this.notIncludedContainer;
    }

    @Override
    public void notIncludedFileFound(File notIncludedFile, String relativePath) {
        // create the target file
        File sourceFile = new File(this.sourceFolder, relativePath);
        File targetFile = new File(this.targetFolder, relativePath);
        if (!notIncludedFile.equals(sourceFile)) {
            throw new IllegalArgumentException("The argument included File, "
                    + "does not fit to the source folder.");
        }

        SourceTargetContainer container = new SourceTargetContainer(sourceFile,
                targetFile);
        this.notIncludedContainer.add(container);
    }

    @Override
    public boolean considerNotIncluded() {
        return true;
    }
}
