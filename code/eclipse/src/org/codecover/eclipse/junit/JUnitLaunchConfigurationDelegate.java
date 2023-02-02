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
import java.net.MalformedURLException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.codecover.eclipse.CodeCoverPlugin;
import org.codecover.eclipse.Messages;
import org.codecover.model.utils.Logger;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.URIUtil;
import org.eclipse.debug.core.ILaunch;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.internal.core.ClasspathEntry;
import org.eclipse.jdt.internal.junit.JUnitCorePlugin;
import org.eclipse.jdt.internal.junit.launcher.ITestKind;
import org.eclipse.jdt.internal.junit.launcher.JUnitLaunchConfigurationConstants;
import org.eclipse.jdt.internal.junit.launcher.JUnitRuntimeClasspathEntry;
import org.eclipse.jdt.internal.junit.launcher.TestKindRegistry;
//import org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate.ClasspathLocalizer;
import org.eclipse.jdt.launching.IRuntimeClasspathEntry;
import org.eclipse.jdt.launching.JavaRuntime;
import org.eclipse.osgi.internal.framework.EquinoxBundle;
import org.eclipse.osgi.internal.loader.EquinoxClassLoader;
import org.eclipse.osgi.util.ManifestElement;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleException;
import org.osgi.framework.Constants;
import org.osgi.framework.wiring.BundleWiring;

import cern.colt.Arrays;


//AGGIUNTI DA PAOLO
import  org.osgi.framework.*;


/**
 * @author Christoph Müller, Igor Podolskiy
 * 
 * @version 1.0 ($Id: JUnitLaunchConfigurationDelegate.java 1 2007-12-12
 *          17:37:26Z t-scheller $)
 * 
 * @see org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate
 */
public class JUnitLaunchConfigurationDelegate extends org.eclipse.jdt.junit.launcher.JUnitLaunchConfigurationDelegate {

	// Logger
	private final Logger logger = CodeCoverPlugin.getDefault().getLogger();
	
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
		System.out.println("***************VVVVVVVVVV");		
		return "org.codecover.juniteclipse.runner.EclipseTestRunner"; //$NON-NLS-1$
	}
	
	@Override
	public String[][] getClasspathAndModulepath(ILaunchConfiguration configuration) throws CoreException {
		System.out.println("**getClasspathAndModulepath");
		
		
		//Da abstractJavaLaunchConfigurationDelegate***********************************************************************************************
		IRuntimeClasspathEntry[] entries = JavaRuntime.computeUnresolvedRuntimeClasspath(configuration);
		entries = JavaRuntime.resolveRuntimeClasspath(entries, configuration);
		String[][] path = new String[2][entries.length];
		List<String> classpathEntries = new ArrayList<>(entries.length);
		List<String> modulepathEntries = new ArrayList<>(entries.length);
		Set<String> classpathSet = new HashSet<>(entries.length);
		Set<String> modulepathSet = new HashSet<>(entries.length);
		for (IRuntimeClasspathEntry entry : entries) {
			String location = entry.getLocation();
			if (location != null) {
				if (entry.getClasspathProperty() == IRuntimeClasspathEntry.MODULE_PATH) {
					if (!modulepathSet.contains(location)) {
						modulepathEntries.add(location);
						modulepathSet.add(location);
						this.logger.info("Add " + location + " to MODULE_PATH");
					}
				} else if (entry.getClasspathProperty() == IRuntimeClasspathEntry.USER_CLASSES
											|| entry.getClasspathProperty() == IRuntimeClasspathEntry.CLASS_PATH) {
					if (!classpathSet.contains(location)) {
						classpathEntries.add(location);
						classpathSet.add(location);
						this.logger.info("Add " + location + " to CLASS_PATH/USER_CLASSES");
					}
				} 
			}
		}
		path[0] = classpathEntries.toArray(new String[classpathEntries.size()]);
		path[1] = modulepathEntries.toArray(new String[modulepathEntries.size()]);
		
		//Da JUnitLaunchConfigurationDelegate***********************************************************************************************
		String[][] cpmp= path;
		String[] cp= cpmp[0];
		
		//****************
		ITestKind kind= JUnitLaunchConfigurationConstants.getTestRunnerKind(configuration);
		if (kind.isNull()) {
			kind= TestKindRegistry.getDefault().getKind(TestKindRegistry.JUNIT3_TEST_KIND_ID); // backward compatible for launch configurations with no runner
		}
		//****************

		//ITestKind kind= getTestRunnerKind(configuration);
		List<String> junitEntries= new ClasspathLocalizer(Platform.inDevelopmentMode()).localizeClasspath(kind);

		String[] classPath= new String[cp.length + junitEntries.size()];
		Object[] jea= junitEntries.toArray();
		System.arraycopy(cp, 0, classPath, 0, cp.length);
		System.arraycopy(jea, 0, classPath, cp.length, jea.length);

		cpmp[0]= classPath;
		
		//Da getClasspath in questo file ***********************************************************************************************
		String[] applicationAndJUnitClasspath = cpmp[0];
		String[] classPathOfPlugin = null;
		try {
			
			//INFO SUI METODI RESTRICTED DA ECLIPSE LUNA IN AVANTI: https://www.eclipse.org/lists/equinox-dev/msg07640.html
			
//			Bundle BundleArray[] = FrameworkUtil.getBundle(getClass()).getBundleContext().getBundles();
			
//			for (Bundle bundle : BundleArray) {
//				System.out.println(bundle.toString());
//			}
			
			Bundle ijjBundle = Platform.getBundle("org.codecover.instrumentation.java.junit"); //$NON-NLS-1$
//			System.out.println("BELLOOOOOO   " +ijjBundle.toString());
//			System.out.println("LOCATIOOON   " +ijjBundle.getLocation());
			
			if (ijjBundle == null) {
				error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null); //$NON-NLS-1$
			}
			
			List<String> _classPathOfPlugin = getClassPath(ijjBundle);
			classPathOfPlugin = new String[_classPathOfPlugin.size()];
			
			//Modifiche Paolo ***********************************************************************************************
			classPathOfPlugin = _classPathOfPlugin.toArray(new String[0]);
			for(int i = 0; i< classPathOfPlugin.length; i++) {
				if (classPathOfPlugin[i].substring(0, 1).compareTo("/")==0 && Platform.getOS().contains("WIN")) 
					classPathOfPlugin[i] = classPathOfPlugin[i].substring(1); //rimuove "file:///" all'inizio di ogni stringa
				classPathOfPlugin[i] = classPathOfPlugin[i].replace("file://", "");
			}
			//fine modifiche paolo ***********************************************************************************************
			
			for (String s: classPathOfPlugin) {
				s = s.replace("/", "\\");
				this.logger.info("Add " + s + " to PLUGIN CLASS_PATH");
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
		cpmp[0] =  adaptedClassPath;
		
		for (String s : adaptedClassPath)
			System.out.println(s);
		
		
//		String[] test = getClasspath(configuration);
//		for(int i = 0; i < adaptedClassPath.length; i++) {
//			System.out.println(adaptedClassPath[i]);
//			System.out.println(test[i]);
//			if(test[i].equals( adaptedClassPath[i])) System.out.println("UGUALI");
//			else System.out.println("DIVERSI");
//			System.out.println();
//		}
		
		return cpmp;
		
	}
	
	
	//Da JUnitLaunchConfigurationDelegate***********************************************************************************************
	private static class ClasspathLocalizer {

		private boolean fInDevelopmentMode;

		public ClasspathLocalizer(boolean inDevelopmentMode) {
			fInDevelopmentMode = inDevelopmentMode;
		}

		public List<String> localizeClasspath(ITestKind kind) {
			JUnitRuntimeClasspathEntry[] entries= kind.getClasspathEntries();
			List<String> junitEntries= new ArrayList<>();

			for (int i= 0; i < entries.length; i++) {
				try {
					addEntry(junitEntries, entries[i]);
				} catch (IOException | URISyntaxException e) {
					Assert.isTrue(false, entries[i].getPluginId() + " is available (required JAR)"); //$NON-NLS-1$
				}
			}
			return junitEntries;
		}

		private void addEntry(List<String> junitEntries, final JUnitRuntimeClasspathEntry entry) throws IOException, MalformedURLException, URISyntaxException {
			String entryString= entryString(entry);
			if (entryString != null)
				junitEntries.add(entryString);
		}

		private String entryString(final JUnitRuntimeClasspathEntry entry) throws IOException, MalformedURLException, URISyntaxException {
			if (inDevelopmentMode()) {
				try {
					return localURL(entry.developmentModeEntry());
				} catch (IOException e3) {
					// fall through and try default
				}
			}
			return localURL(entry);
		}

		private boolean inDevelopmentMode() {
			return fInDevelopmentMode;
		}

		private String localURL(JUnitRuntimeClasspathEntry jar) throws IOException, MalformedURLException, URISyntaxException {
			Bundle bundle= JUnitCorePlugin.getDefault().getBundle(jar.getPluginId());
			URL url;
			if (jar.getPluginRelativePath() == null)
				url= bundle.getEntry("/"); //$NON-NLS-1$
			else
				url= bundle.getEntry(jar.getPluginRelativePath());
			if (url == null)
				throw new IOException();
			return URIUtil.toFile(URIUtil.toURI(FileLocator.toFileURL(url))).getAbsolutePath(); // See bug 503050
		}
	}
	
//	@Override
//	public String[][] getClasspathAndModulepath(ILaunchConfiguration configuration) throws CoreException {
//		System.out.println("**getClasspathAndModulepath");	
//		String[][] cpmp= super.getClasspathAndModulepath(configuration);
//		String[] applicationAndJUnitClasspath = cpmp[0];
//		
//		//Modifiche PAOLO
//		String[] classPathOfPlugin = null;
//		try {
//			System.out.println("***************");
//			Bundle ijjBundle = Platform.getBundle("org.codecover.instrumentation.java.junit"); //$NON-NLS-1$
//			
//			if (ijjBundle == null) {
//				error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_BUNDLE_NOT_FOUND"), null); //$NON-NLS-1$
//			}
//			List<String> _classPathOfPlugin = getClassPath(ijjBundle);
//			classPathOfPlugin = new String[_classPathOfPlugin.size()];
//			
//			classPathOfPlugin = _classPathOfPlugin.toArray(new String[0]);
//			
//		} catch (BundleException e) {
//			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_ERROR"), e); //$NON-NLS-1$
//		} catch (IOException e) {
//			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_IO_ERROR"), e); //$NON-NLS-1$
//		}
//
//		String[] adaptedClassPath = new String[applicationAndJUnitClasspath.length + classPathOfPlugin.length];
//		System.arraycopy(applicationAndJUnitClasspath, 0, adaptedClassPath, 0, applicationAndJUnitClasspath.length);
//		System.arraycopy(classPathOfPlugin, 0, adaptedClassPath, applicationAndJUnitClasspath.length,
//				classPathOfPlugin.length);
//		System.out.println("new classpath " + Arrays.toString(adaptedClassPath));
//		cpmp[0] = adaptedClassPath;
//		
//		//System.out.println("classpath original " + Arrays.toString(cpmp[0]));	
//		//System.out.println("modulepath" + Arrays.toString(cpmp[1]));	
//		//
////		cpmp[0] = getClasspath(configuration);
////		System.out.println("classpath modified" + Arrays.toString(cpmp[0]));	
//		
//		
//		return cpmp;
//}
	@Override
	public String[] getClasspath(ILaunchConfiguration configuration) throws CoreException {
		System.out.println("***************KKKKKKKKK");
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
			String[] _classPathOfPlugin = ijjBundle.getBundleData().getClassPath(); //cercare sostituto
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
			
			classPathOfPlugin = _classPathOfPlugin.toArray(new String[0]);
			System.out.println("GGGGGGGGGGGGGGGGGGGGG");
			
			//for (int i = 0; i < _classPathOfPlugin.size(); i++) {
//			for (int i = 0; i < _classPathOfPlugin.size(); i++) {
//				classPathOfPlugin[i] = FileLocator
//						.toFileURL(FileLocator.find(ijjBundle, new Path(_classPathOfPlugin.get(i)), null)).getFile();
//			}
		} catch (BundleException e) {
			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_ERROR"), e); //$NON-NLS-1$
		} catch (IOException e) {
			error(Messages.getString("JUnitLaunchConfigurationDelegate.RUNNER_PATH_IO_ERROR"), e); //$NON-NLS-1$
		}

		String[] adaptedClassPath = new String[applicationAndJUnitClasspath.length + classPathOfPlugin.length];
		System.arraycopy(applicationAndJUnitClasspath, 0, adaptedClassPath, 0, applicationAndJUnitClasspath.length);
		System.arraycopy(classPathOfPlugin, 0, adaptedClassPath, applicationAndJUnitClasspath.length,
				classPathOfPlugin.length);
		System.out.println("adaptedClassPath   =" + Arrays.toString(adaptedClassPath));
		return adaptedClassPath;
	}

	private void error(String message, Throwable cause) throws CoreException {
		IStatus status = new Status(IStatus.ERROR, CodeCoverPlugin.PLUGIN_ID, message, cause);
		throw new CoreException(status);
	}
	
	
	// TO CHECK SE RSSTITUSCIE FILE:// ....
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
					result.add(/*"file://" +*/ filestring);
					// System.out.println(" added "+"file://"+filestring);
				}
			}
			result.add(getPluginDir(CodeCoverPlugin.PLUGIN_ID).substring(5).replace("!\\", " "));
			result.add(getPluginDir(CodeCoverPlugin.PLUGIN_ID).substring(5).replace("org.codecover.eclipse_2.0.2.jar","org.codecover.instrumentation.java.junit_2.0.2.jar").replace("!", " "));
			result.add(getPluginDir(CodeCoverPlugin.PLUGIN_ID).substring(5).replace("org.codecover.eclipse_2.0.2.jar","org.codecover.instrumentation.java.measurement_2.0.2.jar").replace("!", " "));
			result.add(getPluginDir(CodeCoverPlugin.PLUGIN_ID).substring(5).replace("org.codecover.eclipse_2.0.2.jar","org.codecover.instrumentation.java_2.0.2.jar").replace("!", " "));
			result.add(getPluginDir(CodeCoverPlugin.PLUGIN_ID).substring(5).replace("org.codecover.eclipse_2.0.2.jar","org.codecover.instrumentation_2.0.2.jar").replace("!", " "));
			
			/*result.add("C:\\Users\\Andrea_PC\\Downloads\\eclipse-java-photon-R-win32-x86_64\\eclipse\\plugins\\org.codecover.eclipse_2.0.1.jar");
			result.add("C:\\Users\\Andrea_PC\\Downloads\\eclipse-java-photon-R-win32-x86_64\\eclipse\\plugins\\org.codecover.instrumentation.java.measurement_2.0.1.jar");
			result.add("C:\\Users\\Andrea_PC\\Downloads\\eclipse-java-photon-R-win32-x86_64\\eclipse\\plugins\\org.codecover.instrumentation.java_2.0.1.jar");
			result.add("C:\\Users\\Andrea_PC\\Downloads\\eclipse-java-photon-R-win32-x86_64\\eclipse\\plugins\\org.codecover.instrumentation_2.0.1.jar");*/
		}
		return result;
	}
	
	public static String getPluginDir(String pluginId)
	{
		/* get bundle with the specified id */
		Bundle bundle = Platform.getBundle(pluginId);
		if( bundle == null )
			throw new RuntimeException("Could not resolve plugin: " + pluginId + "\r\n" +
					"Probably the plugin has not been correctly installed.\r\n" +
					"Running eclipse from shell with -clean option may rectify installation.");
		
		/* resolve Bundle::getEntry to local URL */
		URL pluginURL = null;
		try {
			pluginURL = Platform.resolve(bundle.getEntry("/"));
		} catch (IOException e) {
			throw new RuntimeException("Could not get installation directory of the plugin: " + pluginId);
		}
		String pluginInstallDir = pluginURL.getPath().trim();
		if( pluginInstallDir.length() == 0 )
			throw new RuntimeException("Could not get installation directory of the plugin: " + pluginId);
		
		/* since path returned by URL::getPath starts with a forward slash, that
		 * is not suitable to run commandlines on Windows-OS, but for Unix-based
		 * OSes it is needed. So strip one character for windows. There seems
		 * to be no other clean way of doing this. */
		if( Platform.getOS().compareTo(Platform.OS_WIN32) == 0 )
			pluginInstallDir = pluginInstallDir.substring(1);
		
		return pluginInstallDir;
	}
}