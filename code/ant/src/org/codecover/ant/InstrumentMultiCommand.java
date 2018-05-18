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
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.util.FileUtils;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: InstrumentMultiCommand.java 50 2009-06-01 09:59:34Z ahija $)
 *
 */
public class InstrumentMultiCommand extends AInstrumentCommand {

    private static final FileUtils FILE_UTILS = FileUtils.getFileUtils();

    File rootSource;

    File rootTarget;

    List<FileSet> sources = new ArrayList<FileSet>();

    public void setRootSource(File rootSource) {
        this.rootSource = rootSource;
    }

    public void setRootTarget(File rootTarget) {
        this.rootTarget = rootTarget;
    }

    /**
     * Adds a configured {@link FileSet} to this command.
     *
     * @param sourceFileSet
     *                the {@link FileSet} to add.
     */
    public void addConfiguredSource(FileSet sourceFileSet) {
        // wirft eine Exception, wenn der Basis-Pfad des FileSet nicht unter
        // dem RootSource-Pfad ist
        translateSourceToTarget(sourceFileSet.getDir(getProject()));
        this.sources.add(sourceFileSet);
    }

    @Override
    protected void validateBeforeRun() {
        if (this.rootSource == null) {
            throw new BuildException("No <rootSource> element given.");
        }
        if (this.rootTarget == null) {
            throw new BuildException("No <rootTarget> element given.");
        }
        if (this.sources.isEmpty()) {
            throw new BuildException("No <source> element given.");
        }
    }

    @Override
    protected File getSourceDirectory() {
        // the main directory is given by the first fileset
        return this.sources.get(0).getDir(getProject());
    }

    @Override
    protected File getTargetDirectory() {
        // the target directory can be calculated by the first fileset
        return translateSourceToTarget(this.sources.get(0).getDir(getProject()));
    }

    public Collection<SourceTargetContainer> getFilesToInstrument(
            InstrumenterDescriptor descriptor, File rootFolderFile,
            File targetFolderFile) {
        List<SourceTargetContainer> sourceTargetContainers = new ArrayList<SourceTargetContainer>();

        for (FileSet thisFileSet : this.sources) {
            File rootSourceFolder = thisFileSet.getDir(getProject());
            File rootTargetFolder = translateSourceToTarget(rootSourceFolder);

            sourceTargetContainers.addAll(createSourceTargetContainerSet(
                    rootSourceFolder,
                    rootTargetFolder,
                    thisFileSet.getDirectoryScanner(getProject()).getIncludedFiles()));
        }

        return sourceTargetContainers;
    }

    public Collection<SourceTargetContainer> getFilesToCopy(
            InstrumenterDescriptor descriptor, File rootFolderFile,
            File targetFolderFile) {
        List<SourceTargetContainer> sourceTargetContainers = new ArrayList<SourceTargetContainer>();

        for (FileSet thisFileSet : this.sources) {
            File rootSourceFolder = thisFileSet.getDir(getProject());
            File rootTargetFolder = translateSourceToTarget(rootSourceFolder);

            sourceTargetContainers.addAll(createSourceTargetContainerSet(
                    rootSourceFolder,
                    rootTargetFolder,
                    thisFileSet.getDirectoryScanner(getProject()).getNotIncludedFiles()));
        }

        return sourceTargetContainers;
    }

    private File translateSourceToTarget(File pathInSource) {
        if (this.rootSource == null) {
            throw new BuildException("No <rootSource> element given.");
        }
        if (this.rootTarget == null) {
            throw new BuildException("No <rootTarget> element given.");
        }

        String endingPath = removeLeadingPath(this.rootSource, pathInSource);
        return FILE_UTILS.resolveFile(this.rootTarget, endingPath);
    }

    /**
     * @see FileUtils#removeLeadingPath(File, File)
     */
    private static String removeLeadingPath(File leading, File path) {
        String l = FILE_UTILS.normalize(leading.getAbsolutePath()).getAbsolutePath();
        String p = FILE_UTILS.normalize(path.getAbsolutePath()).getAbsolutePath();
        if (l.equals(p)) {
            return "";
        }

        // ensure that l ends with a /
        // so we never think /foo was a parent directory of /foobar
        if (!l.endsWith(File.separator)) {
            l += File.separator;
        }

        if (p.startsWith(l)) {
            return p.substring(l.length());
        }

        throw new BuildException("Path " + p + " is not under " + l);
    }
}
