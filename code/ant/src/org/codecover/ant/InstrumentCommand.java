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

import java.io.File;
import java.util.Collection;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.FileSet;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: InstrumentCommand.java 47 2009-05-25 00:53:41Z ahija $)
 *
 */
public class InstrumentCommand extends AInstrumentCommand {
    File destination;

    FileSet source = null;

    /**
     * Sets the destination.
     *
     * @param destination
     *                the destination to set
     */
    public void setDestination(File destination) {
        this.destination = destination;
    }

    /**
     * Adds a configured {@link FileSet} to this command.
     *
     * @param sourceFileSet
     *                the {@link FileSet} to add.
     */
    public void addConfiguredSource(FileSet sourceFileSet) {
        if (this.source != null) {
            throw new BuildException("There are multiple <source> elements");
        }
        this.source = sourceFileSet;
    }

    @Override
    protected void validateBeforeRun() {
        if (this.source == null) {
            throw new BuildException("The <source> element is missing.");
        }
        if (this.destination == null) {
            throw new BuildException("The attribute 'destination' is missing.");
        }
    }

    @Override
    File getSourceDirectory() {
        return this.source.getDir(getProject());
    }

    @Override
    File getTargetDirectory() {
        return this.destination;
    }

    public Collection<SourceTargetContainer> getFilesToInstrument(
            InstrumenterDescriptor descriptor, File rootFolderFile,
            File targetFolderFile) {
        return createSourceTargetContainerSet(rootFolderFile, targetFolderFile,
                this.source.getDirectoryScanner(getProject()).getIncludedFiles());
    }

    public Collection<SourceTargetContainer> getFilesToCopy(
            InstrumenterDescriptor descriptor, File rootFolderFile,
            File targetFolderFile) {
        return createSourceTargetContainerSet(rootFolderFile, targetFolderFile,
                this.source.getDirectoryScanner(getProject()).getDeselectedFiles());
    }
}
