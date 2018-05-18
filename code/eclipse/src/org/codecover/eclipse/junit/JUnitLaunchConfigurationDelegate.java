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
package org.codecover.eclipse.junit;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.osgi.util.ManifestElement;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;
import org.osgi.framework.Constants;

/**
 * @author Christoph Müller, Igor Podolskiy
 * 
 * @version 1.0 ($Id: JUnitLaunchConfigurationDelegate.java 1 2007-12-12
 *          17:37:26Z t-scheller $)
 * 
 * @see org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate
 */
public class JUnitLaunchConfigurationDelegate extends org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate {

	@Override
	protected void preLaunchCheck(ILaunchConfiguration configuration, ILaunch launch, IProgressMonitor monitor)
			throws CoreException {
		IJavaProject javaProject = super.getJavaProject(configuration);
		if (javaProject != null && !CodeCoverPlugin.isCodeCoverActivated(javaProject.getProject())) {
			IStatus status = new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID,
					Messages.getString("JUnitLaunchConfigurationDelegate.CODECOVER_NOT_ACTIVE")); //$NON-NLS-1$
			throw new CoreException(status);
		}
		// if the project is null, this super - call will throw an CoreException
		super.preLaunchCheck(configuration, launch, monitor);
	}

	@Override
	public String verifyMainTypeName(ILaunchConfiguration configuration) throws CoreException {
		return "org.codecover.juniteclipse.runner.EclipseTestRunner"; //$NON-NLS-1$
	}

	@Override
	public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		String[] applicationAndJUnitClasspath = super.getClasspath(configuration);

		// we extend the classpath of the tests and junit by the classpath
		// of CodeCover JUnit Test Runner (org.codecover.instrumentation.java.junit
		// bundle)
		String[] classPathOfPlugin = null;
		try {
			/*
			 * BundleHost ijjBundle =
			 * (BundleHost)Platform.getBundle("org.codecover.instrumentation.java.junit");
			 * //$NON-NLS-1$
			 * 
			 * if (ijjBundle == null) { error(Messages.getString(
			 * "JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null);
			 * //$NON-NLS-1$ }
			String[] _classPathOfPlugin = ijjBundle.getBundleData().getClassPath();
			 */
			// PROVO A SISTEMARE
			// https://www.eclipse.org/forums/index.php/t/93091/
			//
			System.out.println("***************");
			Bundle ijjBundle = Platform.getBundle("org.codecover.instrumentation.java.junit"); //$NON-NLS-1$
			if (ijjBundle == null) {
				error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null); //$NON-NLS-1$
			}
			System.out.println("***************");
			List<String> _classPathOfPlugin = getClassPath(ijjBundle);
			System.out.println("*****" + _classPathOfPlugin);
			classPathOfPlugin = new String[_classPathOfPlugin.size()];
			//for (int i = 0; i < _classPathOfPlugin.size(); i++) {
			for (int i = 0; i < _classPathOfPlugin.size(); i++) {
				classPathOfPlugin[i] = FileLocator
						.toFileURL(FileLocator.find(ijjBundle, new Path(_classPathOfPlugin.get(i)), null)).getFile();
			}
		} catch (BundleException e) {
			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_ERROR"), e); //$NON-NLS-1$
		} catch (IOException e) {
			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_IO_ERROR"), e); //$NON-NLS-1$
		}

		String[] adaptedClassPath = new String[applicationAndJUnitClasspath.length + classPathOfPlugin.length];
		System.arraycopy(applicationAndJUnitClasspath, 0, adaptedClassPath, 0, applicationAndJUnitClasspath.length);
		System.arraycopy(classPathOfPlugin, 0, adaptedClassPath, applicationAndJUnitClasspath.length,
				classPathOfPlugin.length);
		return adaptedClassPath;
	}

	private void error(String message, Throwable cause) throws CoreException {
		IStatus status = new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, message, cause);
		throw new CoreException(status);
	}

	// ANGELO DA INTERNET
	private static List<String> getClassPath(Bundle bundle) throws IOException, BundleException {
		List<String> result = new ArrayList<String>();
		String requires = bundle.getHeaders().get(Constants.BUNDLE_CLASSPATH);
		if (requires == null)
			requires = ".";
		ManifestElement[] elements = ManifestElement.parseHeader(Constants.BUNDLE_CLASSPATH, requires);
		if (elements != null) {
			for (int i = 0; i < elements.length; ++i) {
				ManifestElement element = elements[i];
				String value = element.getValue();
				if (".".equals(value))
					value = "/";
				URL url = bundle.getEntry(value);
				if (url != null) {
					URL resolvedURL = FileLocator.resolve(url);
					String filestring = FileLocator.toFileURL(resolvedURL).getFile();
					File f = new File(filestring);
					// URL requires trailing / if a directory
					if (f.isDirectory() && !filestring.endsWith("/"))
						filestring += "/";
					result.add("file://" + filestring);
					// System.out.println(" added "+"file://"+filestring);
				}
			}
		}
		return result;
	}
}