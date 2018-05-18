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

import org.codecover.eclipse.CodeCoverPlugin;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchConfigurationWorkingCopy;
import org.eclipse.jdt.launching.IJavaLaunchConfigurationConstants;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.jdt.launching.StandardClasspathProvider;

/**
 * This ClasspathProvider redirects Classpath entries to the corresponding
 * folders which contain the instrumented code.
 * 
 * @author Tilmann Scheller
 * @version 1.0 ($Id: CodeCoverClasspathProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */

public class CodeCoverClasspathProvider extends StandardClasspathProvider {

    /**
     * Value for the
     * {@link IJavaLaunchConfigurationConstants#ATTR_CLASSPATH_PROVIDER}.
     */
    private static final String ATTR_CLASSPATH_PROVIDER_ID = "CodeCoverClasspathProvider"; //$NON-NLS-1$

    @Override
    public IRuntimeClasspathEntry[] computeUnresolvedClasspath(
            ILaunchConfiguration configuration) throws CoreException {
        IRuntimeClasspathEntry[] entries;

        entries = super.computeUnresolvedClasspath(configuration);

        // do nothing in case CodeCover is not enabled for the associated
        // project
        IProject project = JavaRuntime.getJavaProject(configuration)
                .getProject();
        if (!CodeCoverPlugin.isCodeCoverActivated(project)) {
            return entries;
        }

        IRuntimeClasspathEntry[] extendedEntries = new IRuntimeClasspathEntry[entries.length + 1];

        int extra = 1;
        
        for (int i = 1; i < extendedEntries.length; i++) {
            extendedEntries[i] = entries[i - extra];
        }

        // get path to folder with instrumented sources
        IPath filename = CodeCoverPlugin.getDefault().getPathToInstrumentedSources(project);

        extendedEntries[0] = JavaRuntime
                .newArchiveRuntimeClasspathEntry(filename);

        return extendedEntries;
    }

    // accessor methods to enable/disable CodeCover for a particular
    // ILaunchConfiguration, implemented by simply setting the
    // classpath provider attribute of the respective ILaunchConfiguration
    public static boolean isRunningWithCodeCover(ILaunchConfiguration config)
            throws CoreException {
        return config.getAttribute(
                IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER, "") //$NON-NLS-1$
                .equals(ATTR_CLASSPATH_PROVIDER_ID);
    }

    public static void setRunWithCodeCover(
            ILaunchConfigurationWorkingCopy config, boolean run) {
        if (run) {
            config.setAttribute(
                    IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER,
                    ATTR_CLASSPATH_PROVIDER_ID);
        } else {
            config.setAttribute(
                    IJavaLaunchConfigurationConstants.ATTR_CLASSPATH_PROVIDER,
                    (String) null);
        }
    }
}
