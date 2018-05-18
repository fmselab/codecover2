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

import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.util.Arrays;

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
import org.eclipse.osgi.internal.framework.EquinoxBundle;
import org.eclipse.osgi.internal.loader.EquinoxClassLoader;
import org.eclipse.osgi.internal.loader.ModuleClassLoader;
import org.eclipse.osgi.internal.loader.classpath.ClasspathEntry;
import org.eclipse.osgi.launch.Equinox;
//import org.eclipse.osgi.framework.internal.core.BundleHost;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleException;
import org.osgi.framework.BundleReference;
import org.osgi.framework.wiring.BundleWiring;

/**
 * @author Christoph Müller, Igor Podolskiy
 * 
 * @version 1.0 ($Id: JUnitLaunchConfigurationDelegate.java 1 2007-12-12
 *          17:37:26Z t-scheller $)
 * 
 * @see org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate
 */
public class JUnitLaunchConfigurationDelegateWiring extends org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate {

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
		// of CodeCover JUnit Test Runner
		// (org.codecover.instrumentation.java.junit bundle)
		String[] classPathOfPlugin = null;
// ORIGINALE
//      try {
//            BundleHost ijjBundle = (BundleHost)Platform.getBundle("org.codecover.instrumentation.java.junit"); //$NON-NLS-1$
//            if (ijjBundle == null) {
//                            error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null); //$NON-NLS-1$
//            }
//            String[] _classPathOfPlugin = ijjBundle.getBundleData().getClassPath();
//            classPathOfPlugin = new String[_classPathOfPlugin.length];
//            for (int i = 0; i < _classPathOfPlugin.length; i++) {
//                           classPathOfPlugin[i] = FileLocator.toFileURL(FileLocator.find(ijjBundle, new Path(_classPathOfPlugin[i]), null)).getFile();
//            }
//      } catch (BundleException e) {
//                    error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_ERROR"), e); //$NON-NLS-1$
//      } catch (IOException e) {
//                    error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_IO_ERROR"), e); //$NON-NLS-1$
//      }
//		try {
			EquinoxBundle ijjBundle = (EquinoxBundle) Platform.getBundle("org.codecover.instrumentation.java.junit");
			if (ijjBundle == null) {
				error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null); //$NON-NLS-1$
			}
			// USING WIRING
			BundleWiring bundleWiring = ijjBundle.adapt(BundleWiring.class);
			EquinoxClassLoader classLoader = (EquinoxClassLoader) bundleWiring.getClassLoader();
			ClasspathEntry[] _classPathOfPlugin = classLoader.getClasspathManager().getHostClasspathEntries();
			classPathOfPlugin = new String[_classPathOfPlugin.length];
			System.err.println(Arrays.toString(classPathOfPlugin));
			for (int i = 0; i < _classPathOfPlugin.length; i++) {
				classPathOfPlugin[i] = _classPathOfPlugin.toString();				
				//FileLocator.toFileURL(FileLocator.find(ijjBundle,
				//				new Path(_classPathOfPlugin[i].getBundleFile().getBaseFile().getPath()), null))
				//		.getFile();
			}
//		} catch (IOException e) {
//			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_IO_ERROR"), e); //$NON-NLS-1$
///		}
// COME PRIMA
			String[] adaptedClassPath = new String[applicationAndJUnitClasspath.length+ classPathOfPlugin.length];
			System.arraycopy(applicationAndJUnitClasspath,0,adaptedClassPath,0,applicationAndJUnitClasspath.length);
			System.arraycopy(classPathOfPlugin,0,adaptedClassPath,applicationAndJUnitClasspath.length,classPathOfPlugin.length);
			return adaptedClassPath;
	}

	private void error(String message, Throwable cause) throws CoreException {
		IStatus status = new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, message, cause);
		throw new CoreException(status);
	}
}