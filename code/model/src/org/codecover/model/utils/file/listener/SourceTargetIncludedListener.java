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
import java.util.Collections;
import java.util.LinkedList;

import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * This is a {@link FileFoundListener}, which only grabs included files.<br>
 * <br>
 * These files are resolved to a given <code>source folder</code> and a given
 * <code>target folder</code>. Finally this pair of files&mdash;source file
 * and target file&mdash;are packed to a {@link SourceTargetContainer} and added
 * to a {@link Collection}. This collection get be go by
 * {@link #getIncludedSourceTargetContainers()}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: SourceTargetIncludedListener.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SourceTargetIncludedListener extends DummyFileFoundListener {

    private Collection<SourceTargetContainer> includedContainer;

    protected final File targetFolder;

    protected final File sourceFolder;

    /**
     * Constructor for a new {@link SourceTargetIncludedListener}.
     * 
     * @param sourceFolder
     *            The source folder for all included files.
     * @param targetFolder
     *            The target folder for all included files.
     */
    public SourceTargetIncludedListener(File sourceFolder, File targetFolder) {
        this.sourceFolder = sourceFolder;
        this.targetFolder = targetFolder;
        this.includedContainer = new LinkedList<SourceTargetContainer>();
    }

    /**
     * @return The {@link Collection} with all collected included source and
     * target files.
     */
    public Collection<SourceTargetContainer> getIncludedSourceTargetContainers() {
        return this.includedContainer;
    }

    /**
     * @return The {@link Collection} with all collected not included source and
     *         target files.
     */
    public Collection<SourceTargetContainer> getNotIncludedSourceTargetContainers() {
        return Collections.<SourceTargetContainer>emptyList();
    }

    @Override
    public void includedFileFound(File includedFile, String relativePath) {
        // create the target file
        File sourceFile = new File(this.sourceFolder, relativePath);
        File targetFile = new File(this.targetFolder, relativePath);
        if (!includedFile.equals(sourceFile)) {
            throw new IllegalArgumentException("The argument included File, " +
                        "does not fit to the source folder.");
        }

        SourceTargetContainer container = new SourceTargetContainer(sourceFile,
                targetFile);
        this.includedContainer.add(container);
        
    }
}
