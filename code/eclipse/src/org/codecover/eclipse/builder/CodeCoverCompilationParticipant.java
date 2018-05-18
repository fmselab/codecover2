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

package org.codecover.eclipse.builder;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.TreeMap;
import java.util.Map.Entry;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.InstrumentableItemsManager;
import org.codecover.eclipse.tscmanager.ActiveTSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerInfo;
import org.codecover.eclipse.tscmanager.TSContainerManager;
import org.codecover.eclipse.tscmanager.exceptions.CancelException;
import org.codecover.eclipse.tscmanager.exceptions.TSCFileCreateException;
import org.codecover.eclipse.utils.EclipseMASTLinkage;
import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.exceptions.FactoryMisconfigurationException;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.InstrumentationIOException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.FileLoadException;
import org.codecover.model.exceptions.FileSaveException;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.Location;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.utils.file.FileTool;
import org.codecover.model.utils.file.SourceTargetContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragmentRoot;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.core.compiler.BuildContext;
import org.eclipse.jdt.core.compiler.CompilationParticipant;

/**
 * The compilation participants are called by eclipse's java builder to allow
 * plugins to participate on a build. The buildStarting method starts the
 * instrumentation in a .codecover folder in the project folder.<br>
 * <b>Charset:</b> We get the charset of each code file but we use 
 * {@link #DEFAULT_CHARSET_FOR_COMPILING} as the target charset, because all
 * files have to be compiled with the same charset.
 *
 * @author Stefan Franke, Tilmann Scheller, Christoph Müller
 * @version 1.0 ($Id: CodeCoverCompilationParticipant.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class CodeCoverCompilationParticipant extends CompilationParticipant {

    private static final Charset DEFAULT_CHARSET_FOR_COMPILING = Charset.forName("UTF-8"); //$NON-NLS-1$

    private static final boolean SEARCH_IN_ALL_TSC = false;

    private final Logger eclipseLogger = CodeCoverPlugin.getDefault().getLogger();
    
    
    @Override
    public int aboutToBuild(IJavaProject project) {
        // until the instrumenter can handle incremental instrumentation we need to do full builds
        return CompilationParticipant.NEEDS_FULL_BUILD;
    }

    /**
     * This method is called for each project separately.
     */
    @Override
    public void buildStarting(BuildContext[] files, boolean isBatch) {
        if (files.length == 0) {
            return;
        }

        final IProject iProject = files[0].getFile().getProject();
        final IPath projectFullPath = iProject.getFullPath();
        final IPath projectLocation = iProject.getLocation();
        final String projectFolderLocation = iProject.getLocation().toOSString();
        final Queue<SourceTargetContainer> sourceTargetContainers = new LinkedList<SourceTargetContainer>(); 
        final String instrumentedFolderLocation = CodeCoverPlugin.getDefault().getPathToInstrumentedSources(iProject).toOSString(); 

        // try to get all source folders
        final Queue<String> possibleSourceFolders = new LinkedList<String>();
        try {
            final IJavaProject javaProject = JavaCore.create(iProject);
            for (IPackageFragmentRoot thisRoot : javaProject
                    .getAllPackageFragmentRoots()) {
                IResource resource = thisRoot.getCorrespondingResource();
                if (resource != null) {
                    possibleSourceFolders.add(resource.getLocation()
                            .toOSString());
                }
            }
        } catch (JavaModelException e) {
            eclipseLogger.fatal(
                    "JavaModelException in CompilationParticipant", e); //$NON-NLS-1$
            return;
        }

        InstrumentableItemsManager instrumentableItemsManager = CodeCoverPlugin
            .getDefault().getInstrumentableItemsManager();
        TSContainerManager tscManager = CodeCoverPlugin.getDefault().getTSContainerManager();
        
        // //////////////////////////////////////////////////////////////////////
        // CREATE THE SOURCE TARGET CONTAINERS AND COPY THE UNINSTRUMENTED
        // FILES TO THE INSTRUMENTEDFOLDERLOCATION
        // //////////////////////////////////////////////////////////////////////
        fillSourceTargetContainers(
                files,
                possibleSourceFolders,
                sourceTargetContainers,
                instrumentedFolderLocation,
                eclipseLogger,
                instrumentableItemsManager);

        // //////////////////////////////////////////////////////////////////////
        // SEARCH IN ALL TSC OF THIS PROJECT IF A TSC CAN BE REUSED
        // //////////////////////////////////////////////////////////////////////
        
        TestSessionContainer tsc;
        if (SEARCH_IN_ALL_TSC) {
            tsc = searchUseableTSC(iProject,
                                   files,
                                   instrumentableItemsManager,
                                   tscManager);
        } else {
            tsc = getUseableTSC(files,
                                instrumentableItemsManager,
                                tscManager);
        }
        
        // //////////////////////////////////////////////////////////////////////
        // PREPARE INSTRUMENTATION
        // //////////////////////////////////////////////////////////////////////
        InstrumenterDescriptor descriptor = new org.codecover.instrumentation.java15.InstrumenterDescriptor();
        if (descriptor == null) {
            eclipseLogger.fatal("NullPointerException in CompilationParticipant"); //$NON-NLS-1$
        }

        // check whether TSC's criteria match
        // with the selected criteria of the project
        if (tsc != null) {
           Set<Criterion> tscCriteria = tsc.getCriteria();

           for (Criterion criterion : tscCriteria) {
               if (!CodeCoverPlugin.getCriterionSelectedState(iProject, criterion)) {
                   // the TSC uses a criterion which is not selected for the project
                   // therefore it can't be used
                   tsc = null;
               }
           }

           // all selected criteria must be active for the TSC
           for (Criterion criterion : descriptor.getSupportedCriteria()) {
               if (CodeCoverPlugin.getCriterionSelectedState(iProject, criterion)) {
                   if (!tscCriteria.contains(criterion)) {
                       // the TSC doesn't use a criterion which is selected
                       // for the project, this means we can't use the TSC
                       tsc = null;
                   }
               }
           }
        }

        eclipseLogger.debug("can reuse TSC: " + (tsc != null ? tsc : "no")); //$NON-NLS-1$ //$NON-NLS-2$

        InstrumenterFactory factory = new DefaultInstrumenterFactory();
        factory.setDescriptor(descriptor);
        
        // only instrument with the selected criteria
        for (Criterion criterion : descriptor.getSupportedCriteria()) {
            if (CodeCoverPlugin.getCriterionSelectedState(iProject, criterion)) {
                factory.addCriterion(criterion);
            }
        }
        
        factory.setCharset(DEFAULT_CHARSET_FOR_COMPILING);

        Instrumenter instrumenter = null;
        try {
            instrumenter = factory.getInstrumenter();
        } catch (FactoryMisconfigurationException e) {
            eclipseLogger.fatal("FactoryMisconfigurationException in CompilationParticipant"); //$NON-NLS-1$
        }

        // //////////////////////////////////////////////////////////////////////
        // INSTRUMENT
        // //////////////////////////////////////////////////////////////////////
        File rootFolder = new File(projectFolderLocation);
        File targetFolder = new File(instrumentedFolderLocation);
        MASTBuilder builder = new MASTBuilder(eclipseLogger);
        Map<String, Object> instrumenterDirectives = descriptor
                .getDefaultDirectiveValues();
        CodeCoverPlugin plugin = CodeCoverPlugin.getDefault();
        eclipseLogger.debug("Plugin: " + plugin);
        IPath coverageLogPath = CodeCoverPlugin.getDefault().getPathToCoverageLogs(iProject);
        coverageLogPath = coverageLogPath.append("coverage-log-file.clf"); //$NON-NLS-1$
        
        instrumenterDirectives
                .put(
                        org.codecover.instrumentation.java15.InstrumenterDescriptor.CoverageLogPathDirective.KEY,
                        coverageLogPath.toOSString());
        
        if (tsc != null) {
            // we can reuse the TSC
            instrumenterDirectives.put(
                    org.codecover.instrumentation.UUIDDirective.KEY, tsc
                            .getId());
        }

        TestSessionContainer testSessionContainer;
        try {
            testSessionContainer = instrumenter.instrument(rootFolder,
                    targetFolder, sourceTargetContainers, builder,
                    instrumenterDirectives);
        } catch (InstrumentationIOException e) {
            eclipseLogger.fatal(
                    "InstrumentationIOException in CompilationParticipant", e); //$NON-NLS-1$
            return;
        } catch (InstrumentationException e) {
            eclipseLogger.fatal(
                    "InstrumentationException in CompilationParticipant", e); //$NON-NLS-1$
            return;
        }

        // //////////////////////////////////////////////////////////////////////
        // SAVE TSC
        // //////////////////////////////////////////////////////////////////////
        if (tsc == null) {
            // we have to create a new TSC
            try {
                tscManager.addTestSessionContainer(testSessionContainer,
                        iProject, false, null, null);
            } catch (FileSaveException e) {
                eclipseLogger.fatal(
                        "FileSaveException in CompilationParticipant", e); //$NON-NLS-1$
            } catch (TSCFileCreateException e) {
                eclipseLogger.fatal(
                        "CoreException in CompilationParticipant", e); //$NON-NLS-1$
            } catch (FileLoadException e) {
                eclipseLogger.fatal(
                        "CoreException in CompilationParticipant", e); //$NON-NLS-1$
            } catch (InvocationTargetException e) {
                // can't happen because we didn't pass a runnable
                eclipseLogger.warning(
                        "InvocationTargetException in CompilationParticipant", e); //$NON-NLS-1$
            } catch (CancelException e) {
                eclipseLogger.warning("User canceled writing of" + //$NON-NLS-1$
                                " new test session container in" +         //$NON-NLS-1$
                                " CompilationParticipant");                //$NON-NLS-1$
            }
        }

        // TODO handle compilation errors
        IJavaProject javaProject = JavaCore.create(iProject);

        // set up classpath
        StringBuilder runCommand = new StringBuilder(1024);
        IClasspathEntry[] cpEntries;
        try {
            cpEntries = javaProject.getResolvedClasspath(true);
        } catch (JavaModelException e) {
            eclipseLogger.fatal(
                    "JavaModelException in CompilationParticipant", e); //$NON-NLS-1$
            return;
        }

        for (int i = 0; i < cpEntries.length; i++) {
            IClasspathEntry thisEntry = cpEntries[i];
            if (thisEntry.getEntryKind() == IClasspathEntry.CPE_LIBRARY) {
                if (runCommand.length() == 0) {
                    // this is the first entry -> create the class path option
                    runCommand.append("-cp "); //$NON-NLS-1$
                } else {
                    // this is not the first -> we need a separator
                    runCommand.append(File.pathSeparatorChar);
                }
                runCommand.append("\""); //$NON-NLS-1$
                IPath itsIPath = thisEntry.getPath();
                if (projectFullPath.isPrefixOf(itsIPath)) {
                    itsIPath = itsIPath.removeFirstSegments(1);
                    itsIPath = projectLocation.append(itsIPath);
                }
                runCommand.append(itsIPath.toOSString());
                runCommand.append("\""); //$NON-NLS-1$
            }
        }

        // check java version related options
        String targetVersion = javaProject.getOption(
                JavaCore.COMPILER_CODEGEN_TARGET_PLATFORM, true);
        runCommand.append(" -target "); //$NON-NLS-1$
        runCommand.append(targetVersion);
        String sourceVersion = javaProject.getOption(JavaCore.COMPILER_SOURCE,
                true);
        runCommand.append(" -source "); //$NON-NLS-1$
        runCommand.append(sourceVersion);

        // no warnings
        runCommand.append(" -nowarn"); //$NON-NLS-1$

        // use the default charset for the encoding
        // all files have been instrumented or copied using this charset
        runCommand.append(" -encoding "); //$NON-NLS-1$
        runCommand.append(DEFAULT_CHARSET_FOR_COMPILING.toString());

        // the directory to compile
        // put the path in "", because the commandline tokenizes this path
        runCommand.append(" \""); //$NON-NLS-1$
        runCommand.append(instrumentedFolderLocation);
        runCommand.append("\""); //$NON-NLS-1$

        eclipseLogger.debug("I run this compile command now:\n" + runCommand); //$NON-NLS-1$
        StringWriter out = new StringWriter();
        StringWriter err = new StringWriter();
        boolean result;
        result = org.eclipse.jdt.internal.compiler.batch.Main.compile(runCommand.toString(), new PrintWriter(out), new PrintWriter(err));
        
        eclipseLogger.debug("ECJ Output: " + out.toString()); //$NON-NLS-1$
        eclipseLogger.debug("ECJ Error Output: " + err.toString()); //$NON-NLS-1$

        if (!result) {
            eclipseLogger.fatal("An error occured when trying to compile the instrumented sources."); //$NON-NLS-1$
        }
        
        super.buildStarting(files, isBatch);
    }

    private void fillSourceTargetContainers(
            final BuildContext[] files,
            final Queue<String> possibleSourceFolders,
            final Collection<SourceTargetContainer> sourceTargetContainers,
            final String instrumentedFolderLocation,
            final Logger eclipseLogger,
            final InstrumentableItemsManager instrumentableItemsManager) {

        for (BuildContext buildContext : files) {
            final IFile iFile = buildContext.getFile();
            final String sourceFileLocation = iFile.getLocation()
                    .makeAbsolute().toOSString();

            // try to find the related source folder (IPackageFragmentRoot)
            // we do not know, which source folder is the correct one, because
            // you can have source folders in source folders, so we get the one
            // with the longest fit to sourceFileLoacation
            String sourceFolderWithBestFit = null;
            for (String thisSourceFolder : possibleSourceFolders) {
                if (sourceFileLocation.startsWith(thisSourceFolder)) {
                    // this might be the related source folder
                    if (sourceFolderWithBestFit == null
                            || thisSourceFolder.length() > sourceFolderWithBestFit.length()) {
                        sourceFolderWithBestFit = thisSourceFolder;
                    }
                }
            }
            if (sourceFolderWithBestFit == null) {
                eclipseLogger.fatal("We try to instrument a file, that is not under a source folder"); //$NON-NLS-1$
            }

            // we get the relative path under the source folder and add it to
            // the target of the instrumentation folder
            String instrumentFileLocation = instrumentedFolderLocation
                    + sourceFileLocation.substring(sourceFolderWithBestFit.length());

            File sourceFile = new File(sourceFileLocation);
            File instrumentFile = new File(instrumentFileLocation);

            // try to get the charset of this file
            Charset charsetOfFile = null;
            try {
                charsetOfFile = Charset.forName(iFile.getCharset());
            } catch (CoreException e) {
                e.printStackTrace();
                charsetOfFile = DEFAULT_CHARSET_FOR_COMPILING;
            }
            if (instrumentableItemsManager.containsIPath(iFile.getFullPath())) {
                // has to be instrumented!
                SourceTargetContainer sourceTargetContainer = new SourceTargetContainer(
                        sourceFile, instrumentFile, charsetOfFile);
                sourceTargetContainers.add(sourceTargetContainer);
            } else {
                // not selected for instrumentation ->
                // copy it by using the defaultCharset for the target
                try {
                    FileTool.copy(sourceFile,     charsetOfFile,
                                  instrumentFile, DEFAULT_CHARSET_FOR_COMPILING);
                } catch (IOException e) {
                    eclipseLogger.fatal(
                            "IOException in CompilationParticipant", e); //$NON-NLS-1$
                }
            }
        }
    }

    private TestSessionContainer searchUseableTSC(IProject project,
            BuildContext[] files,
            InstrumentableItemsManager instrumentableItemsManager,
            TSContainerManager tscManager) {
        List<TSContainerInfo> allTSContainerInfos = tscManager.getTestSessionContainers(project);
        if (allTSContainerInfos.isEmpty()) {
            // no TSC found for this project
            return null;
        }

        // sort the TSCInfo by their date descending
        TreeMap<Date, TSContainerInfo> descendingTSC = new TreeMap<Date, TSContainerInfo>(new Comparator<Date>() {
            @Override
			public int compare(Date date1, Date date2) {
                return -1 * date1.compareTo(date2);
            }
        });
        for (TSContainerInfo thisTSCInfo : allTSContainerInfos) {
            descendingTSC.put(thisTSCInfo.getDate(), thisTSCInfo);
        }

        // now, we open all TestSessionContainers to check, whether it is 
        // the one, we can use
        
        // save the current TSC
        ActiveTSContainerInfo activeTSCBefore = tscManager.getActiveTSContainer();
        TSContainerInfo activeInfoBefore = null;
        if (activeTSCBefore != null) {
            activeInfoBefore = activeTSCBefore.getTSContainerInfo(); 
        }

        TestSessionContainer tscToUse = null;
        tscLoop : for (Entry<Date,TSContainerInfo> thisEntry : descendingTSC.entrySet()) {
            TSContainerInfo thisInfo = thisEntry.getValue();
            tscToUse = getUseableTSC(thisInfo, files, tscManager, instrumentableItemsManager); 
            if (tscToUse != null) {
                // found a useable tsc
                break tscLoop;
            }
        }

        // restore the old TSC
        ActiveTSContainerInfo activeTSCNow = tscManager.getActiveTSContainer();
        TSContainerInfo activeInfoNow = null;
        if (activeTSCNow != null) {
            activeInfoNow = activeTSCNow.getTSContainerInfo(); 
        }
        if ((activeInfoBefore == null && activeInfoNow != null) ||
            (activeInfoBefore != null && activeInfoNow == null) ||
            (activeInfoBefore != null && activeInfoNow != null &&
             !activeInfoBefore.equals(activeInfoNow))) {
            // we have to restore the TSC before

            try {
                tscManager.setActiveTSContainer(activeInfoBefore,
                        null,
                        null);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        // nothing found
        return tscToUse;
    }

    /**
     * It is checked, whether the {@link TestSessionContainer#getCode()} of
     * the {@link ActiveTSContainerInfo} is equal to the <code>files</code>.<br>
     * If yes, the {@link TestSessionContainer} is returned&mdash;<code>null</code> else.
     */
    private TestSessionContainer getUseableTSC(BuildContext[] files,
            InstrumentableItemsManager instrumentableItemsManager,
            TSContainerManager tscManager) {
        ActiveTSContainerInfo activeTSC = tscManager.getActiveTSContainer();
        if (activeTSC != null && activeTSC.getTSContainerInfo() != null) {
            return getUseableTSC(
                    activeTSC.getTSContainerInfo(),
                    files,
                    tscManager,
                    instrumentableItemsManager);
        } else {
            return null;
        }
    }

    /**
     * If the given {@link TSContainerInfo} is not the {@link ActiveTSContainerInfo}
     * than this it is opened. Afterwards it is checked, whether its 
     * {@link TestSessionContainer#getCode()} is equal to the <code>files</code>.<br>
     * If yes, the {@link TestSessionContainer} is returned&mdash;<code>null</code> else.
     * @param tscManager TODO
     */
    private TestSessionContainer getUseableTSC(TSContainerInfo thisInfo,
            BuildContext[] files,
            TSContainerManager tscManager,
            InstrumentableItemsManager instrumentableItemsManager) {
        ActiveTSContainerInfo activeTSC = tscManager.getActiveTSContainer();
        TSContainerInfo activeInfo = null;
        if (activeTSC != null) {
            activeInfo = activeTSC.getTSContainerInfo();
        }

        if (activeInfo == null || !activeInfo.equals(thisInfo)) {
            // have to open TSC first
            try {
                tscManager.setActiveTSContainer(thisInfo,
                        null,
                        null);
            } catch (Exception e) {
                return null;
            }
        }

        activeTSC = tscManager.getActiveTSContainer();
        TestSessionContainer tsc = null;
        activeInfo = null;
        if (activeTSC != null) {
            tsc = activeTSC.getTestSessionContainer();
            activeInfo = activeTSC.getTSContainerInfo(); 
        }
        if (activeInfo == null || tsc == null || !activeInfo.equals(thisInfo)) {
            // opening went wrong
            return null;
        }

        // assert: we have thisInfo opened
        
        eclipseLogger.debug("check TSC " + tsc); //$NON-NLS-1$
        if (isCodeUnchanged(tsc.getCode(), files, instrumentableItemsManager)) {
            return tsc;
        } else {
            return null;
        }
    }

    private boolean isCodeUnchanged(HierarchyLevel code, BuildContext[] files,
            InstrumentableItemsManager instrumentableItemsManager) {
        boolean result = true;
        // check whether we need a new TSC
        // this is done by checking every file
        for (BuildContext buildContext : files) {
            final IFile file = buildContext.getFile();
            final IJavaElement compilationUnit = JavaCore.create(file);

            // find the source file, which contains the public class
            // with the same name the "file" has got
            final HierarchyLevel codeFile = EclipseMASTLinkage.findSource(code,
                    compilationUnit);

            final boolean instrument = instrumentableItemsManager
                    .containsIPath(file.getFullPath());
            final boolean fileInTSC = codeFile != null;

            if (instrument && fileInTSC) {
                String mastText;
                Location mastLocation;

                // get the source file of the location
                mastLocation = codeFile.getLocation().getLocations().get(0);
                mastText = mastLocation.getFile().getContent();
                // FIXME deal with unsynced files
                try {
                    InputStream inStream = file.getContents();
                    Charset charsetOfFile = Charset.forName(file.getCharset());
                    String contentOfFile = FileTool.getContentFromStream(
                            inStream, charsetOfFile);

                    if (!mastText.equals(contentOfFile)) {
                        // code file is changed to the MAST
                        eclipseLogger.debug(file.getName()
                                + " has changed (break)"); //$NON-NLS-1$
                        result = false;
                        break;
                    }
                } catch (CoreException e) {
                    e.printStackTrace();
                    return false;
                } catch (IOException e) {
                    e.printStackTrace();
                    return false;
                }
            } else if (instrument && !fileInTSC) {
                // codeFile == null -> code file not in MAST
                eclipseLogger.debug("was not found in the MAST (break)"); //$NON-NLS-1$
                result = false;
                break;
            } else if (!instrument && fileInTSC) {
                // checking of files which are not to be instrumented
                // since the TSC can only contain files which are to be
                // instrumented, we need a new TSC in case it contains
                // entries for files which are not to be instrumented
                // (because they obviously will never be covered and
                // therefore distort the calculated coverage metrics)

                // file which is not to be instrumented is part of the TSC
                result = false;
                break;
            }
        }

        // every check was successful
        return result;
    }

    @Override
    public void cleanStarting(IJavaProject project) {
        eclipseLogger.debug("cleaning"); //$NON-NLS-1$
        File filename = CodeCoverPlugin.getDefault().getPathToInstrumentedSources(project.getProject()).toFile();
        deleteDir(filename);
    }

    private boolean deleteDir(File dir) {
        // to see if this directory is actually a symbolic link to a directory,
        // we want to get its canonical path - that is, we follow the link to
        // the file it's actually linked to
        File candir;
        try {
            candir = dir.getCanonicalFile();
        } catch (IOException e) {
            return false;
        }

        // a symbolic link has a different canonical path than its actual path,
        // unless it's a link to itself
        if (!candir.equals(dir.getAbsoluteFile())) {
            // this file is a symbolic link, and there's no reason for us to
            // follow it, because then we might be deleting something outside of
            // the directory we were told to delete
            return false;
        }

        // now we go through all of the files and subdirectories in the
        // directory and delete them one by one
        File[] files = candir.listFiles();
        if (files != null) {
            for (int i = 0; i < files.length; i++) {
                File file = files[i];

                // in case this directory is actually a symbolic link, or it's
                // empty, we want to try to delete the link before we try
                // anything
                boolean deleted = file.delete();
                if (!deleted) {
                    // deleting the file failed, so maybe it's a non-empty
                    // directory
                    if (file.isDirectory())
                        deleteDir(file);

                    // otherwise, there's nothing else we can do
                }
            }
        }

        // now that we tried to clear the directory out, we can try to delete it
        // again
        return dir.delete();
    }

    @Override
    public boolean isActive(IJavaProject project) {
        // only handle projects which are actually enabled for use with CodeCover
        return CodeCoverPlugin.isCodeCoverActivated(project.getProject());
    }

}
