/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.model.extensions;

import java.io.*;
import java.net.*;
import java.util.*;
import java.text.*;
import java.security.*;
import org.w3c.dom.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import javax.xml.parsers.*;
import java.util.regex.*;
import java.util.jar.*;

import org.codecover.model.utils.*;

/**
 * Plugin utils.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: PluginUtils.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PluginUtils {
    private PluginUtils() {
    }
    
    public static <T> Set<T> getExtensionObjects(PluginManager manager, Logger logger, Class<T> interfaceType) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (interfaceType == null) {
            throw new NullPointerException("interfaceType == null");
        }

        final Set<T> result = new HashSet<T>();
        
        for (final Extension<T> extension : getExtensions(manager, logger, interfaceType)) {
            try {
                result.add(extension.getObject());
            } catch (Exception e) {
                logger.error("Error creating object for extension " + extension.getClass(), e);
            }
        }

        return Collections.unmodifiableSet(result);
    }

    public static <T> Set<Extension<T>> getExtensions(PluginManager manager, Logger logger, Class<T> interfaceType) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (interfaceType == null) {
            throw new NullPointerException("interfaceType == null");
        }

        final Set<Extension<T>> result = new HashSet<Extension<T>>();
        
        for (final PluginHandle handle : manager.getPluginHandles()) {
            try {
                result.addAll(handle.getPlugin().getExtensions(interfaceType));
            } catch (PluginLoadException e) {
                logger.error("Error loading extensions of plugin " + handle.getPluginName() + ": " + e.getMessage(), e);
            }
        }
        
        return Collections.unmodifiableSet(result);
    }
    
    public static void loadPluginsFromPluginDirectory(PluginManager manager, Logger logger) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        
        File pluginDir = getDefaultPluginDirectory(logger);

        if (pluginDir != null) {
            loadPluginsFromDirectory(manager, logger, pluginDir);
        }
    }

    /**
     * Tries to find the default plugin directory, returns {@code null}
     * if it can't find it.
     */
    public static File getDefaultPluginDirectory(Logger logger) {
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        
        return getPluginDirectory(logger, getPathOfCore(logger));
    }

    /**
     * Returns the location of a plugin directory which is in the same
     * same directory as the link target of {@code jarLocation}
     * and is called "plugins".
     *
     * Returns {@code null} if {@code jarLocation} is {@code null}.
     */
    public static File getPluginDirectory(Logger logger, File jarLocation) {
        if (jarLocation == null) {
            return null;
        }
        
        final File realFile;
        // Follow symlinks etc.
        try {
            realFile = jarLocation.getCanonicalFile();
        } catch (IOException e) {
            logger.error("Got IOException looking up my jar path", e);
            return null;
        }

        final File parent = realFile.getParentFile();
        if (parent == null) {
            // This means e.g. that we were started from /
            // Simply ignore this case (it's idiotic anyway)
            return null;
        }

        File file = new File(parent, "plugins/");
        
        if (!file.exists()) {
            return null;
        }
        
        return file;
    }
    
    /**
     * Tries to find the path of the CodeCover core jar, returns {@code null}
     * if it can't find it.
     */
    public static File getPathOfCore(Logger logger) {
        return getPathOfClass(logger, PluginUtils.class);
    }
    
    // Check, whether myPath is in the dir / jar path
    private static boolean checkPath(File file, String myJarPath, String myPath) {
        if (file.isDirectory()) { // dir
            if (new File(file, myPath).exists()) {
                // We're in this dir. We hope.
                return true;
            }
        } else { // jar file
            try { 
                final JarFile jarFile = new JarFile(file);
                boolean exists = jarFile.getEntry(myJarPath) != null;
                jarFile.close();
                if (exists) {
                    // We're in this jar.
                    return true;
                }
            } catch (IllegalStateException e) { // ignore
            } catch (IOException e) { // ignore
            } catch (SecurityException e) { // ignore
            }
        }
        
        return false;
    }
    
    public static interface ClassLoaderHandler {
        // Returns a list of possible locations or null if we don't know
        // about this classloader
        public List<File> handleClassLoader(Logger logger, ClassLoader classLoader);
    }
    
    private static final ClassLoaderHandler urlClassLoaderHandler = new ClassLoaderHandler() {
            public List<File> handleClassLoader(Logger logger, ClassLoader classLoader) {
                if (!(classLoader instanceof URLClassLoader)) {
                    return null;
                }

                final URLClassLoader urlClassLoader = (URLClassLoader) classLoader;
                
                final List<File> result = new ArrayList<File>();
                
                for (final URL url : urlClassLoader.getURLs()) {
                    final URI uri; 
                    try {
                        uri = url.toURI();
                    } catch (URISyntaxException e) {
                        // Hopefully java won't create a malformed URI
                        throw new RuntimeException(e);
                    }
                    if (!uri.getScheme().equals("file")) {
                        logger.warning("Cannot load plugins because we are loaded from a " + uri.getScheme() + ": URI.");
                        return null;
                    }
                    result.add(new File(uri));
                }
                
                return result;
            }
        };
    
    // When the class name is a.B an it is in /asdf/a/B.class, we will return
    // /asdf/
    /**
     * Tries to find the path of the jar or directory containing {@code clss},
     * returns {@code null} if it can't find it.
     */
    public static File getPathOfClass(Logger logger, Class clss) {
        return getPathOfClass(logger, clss, Collections.<ClassLoaderHandler>emptyList());
    }

    /**
     * Tries to find the path of the jar or directory containing {@code clss},
     * returns {@code null} if it can't find it.
     */
    public static File getPathOfClass(Logger logger, Class clss, List<ClassLoaderHandler> classLoaderHandlers) {
        final List<ClassLoaderHandler> myClassLoaderHandlers = new ArrayList<ClassLoaderHandler>(classLoaderHandlers);
        myClassLoaderHandlers.add(urlClassLoaderHandler);

        final String myJarPath = clss.getName().replace('.', '/') + ".class"; // TODO: should we use separatorChar (or '/') for JarFile? http://java.sun.com/j2se/1.5.0/docs/api/java/util/jar/JarFile.html#getEntry(java.lang.String) doesn't care about such nasty details. However, it seems like '\' doesn't work on Windows but '/' does. So we just use '/'.
        final String myPath = clss.getName().replace('.', File.separatorChar) + ".class";
        
        final ClassLoader myClassLoader = clss.getClassLoader();

        if (myClassLoader == null) {
            logger.warning("Cannot find plugin directory (classloader is null)");
            return null;
        }
        
        for (final ClassLoaderHandler handler : myClassLoaderHandlers) {
            final List<File> files = handler.handleClassLoader(logger, myClassLoader);
            
            if (files != null) {
                for (final File file : files) {
                    if (checkPath (file, myJarPath, myPath)) {
                        return file;
                    }
                }
                
                logger.warning("Cannot find plugin directory (cannot find jar)");
                return null;
            }
        }
        
        // We have found no known ClassLoader. Try something else.
        
        final CodeSource codeSource = clss.getProtectionDomain().getCodeSource();
        if (codeSource != null) {
            final URI uri;
            try {
                uri = codeSource.getLocation().toURI();
            } catch (URISyntaxException e) {
                // Hopefully java won't create a malformed URI
                throw new RuntimeException(e);
            }
            if (!uri.getScheme().equals("file")) {
                logger.error("Cannot load plugins because we are loaded from a " + uri.getScheme() + ": URI.");
                return null;
            }
            final File file = new File(uri);
            if (checkPath(file, myJarPath, myPath)) {
                return file;
            }
        }
        
        // failed.
        
        logger.warning("Cannot find plugin directory (classloader is of unkown type '" + myClassLoader.getClass().getName() + "')");
        return null;

    }
    
    private static final Pattern fileNamePattern = Pattern.compile("(.*)-([0-9]*)\\.([0-9]*)\\.jar");
    
    // File have to named "pluginname-<major>.<minor>.jar"
    public static void loadPluginsFromDirectory(PluginManager manager, Logger logger, final File directory) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (directory == null) {
            throw new NullPointerException("directory == null");
        }

        final String[] files = directory.list();
        
        if (files == null) {
            logger.error("The plugin directory '" + directory.getAbsolutePath() + "' is no directory or an IO error occured during reading it.");
            return;
        }
        
        final Set<String> filesToLoad = new TreeSet<String>();
        final Map<Pair<String, Integer>, Pair<Integer, String>> nameIndex = new HashMap<Pair<String, Integer>, Pair<Integer, String>>();
        
        for (final String filename : files) {
            Matcher m = fileNamePattern.matcher(filename);
            if (m.matches()) {
                String name = m.group(1);
                String smajor = m.group(2);
                String sminor = m.group(3);
                final int major, minor;
                try {
                    major = Integer.parseInt(smajor);
                    minor = Integer.parseInt(sminor);
                } catch (NumberFormatException e) {
                    continue;
                }
                filesToLoad.add(filename);
                final Pair<Integer, String> actual = nameIndex.get(new Pair<String, Integer>(name, major));
                if (actual == null || actual.first < minor) {
                    nameIndex.put(new Pair<String, Integer>(name, major), new Pair<Integer, String>(minor, filename));
                }
            } else {
                // Hmm. What priority should we use here? Let's simply assume
                // that the plugin directory ist clobbered with other files
                // anyway...
                logger.debug("Ignoring file '" + filename + "' in plugin directory (has not format pluginname-<major>.<minor>.jar)");
            }
        }
        
        for (final Pair<Integer, String> pair : nameIndex.values()) {
            filesToLoad.add(pair.second);
        }
        
        final PluginManager.PluginNameResolver nameResolver = new PluginManager.PluginNameResolver() {
                public File resolve(String name, int majorVersion) throws PluginLoadException {
                    final Pair<Integer, String> pair = nameIndex.get(Pair.create(name, majorVersion));
                    if (pair == null) {
                        throw new PluginLoadException("Cannot find plugin with name " + name + " and major version " + majorVersion);
                    }
                    filesToLoad.remove(pair.second);
                    return new File(directory, pair.second);
                }
            };
        
        for (final String filename : files) {
            if (filesToLoad.contains(filename)) {
                try {
                    final PluginHandle handle = manager.loadPlugin (nameResolver, new File(directory, filename));
                    manager.addPlugin(handle);
                } catch (NoCodecoverPluginException e) {
                    // This means that we tried to load something that is
                    // no plugin. Doesn't really matter.
                    logger.debug("Error loading plugin " + filename + ": " + e, e);
                } catch (PluginLoadException e) {
                    logger.error("Error loading plugin " + filename + ": " + e, e);
                }
            }
        }
    }
    
    public static PluginHandle getNewestPluginWithName(PluginManager manager, Logger logger, String pluginName) {
        PluginHandle handle = tryGetNewestPluginWithName(manager, logger, pluginName);
        if (handle == null) {
            logger.fatal("Could not find plugin " + pluginName);
            throw new RuntimeException();
        }
        return handle;
    }

    public static PluginHandle tryGetNewestPluginWithName(PluginManager manager, Logger logger, String pluginName) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (pluginName == null) {
            throw new NullPointerException("pluginName == null");
        }

        PluginHandle pluginHandle = null;
        int foundMajorVersion = -1;
        int foundMinorVersion = -1;
        
        for (final PluginHandle handle : manager.getPluginHandles()) {
            if (handle.getPluginName().equals(pluginName)) {
                if (foundMajorVersion < handle.getPluginVersionMajor()) {
                    pluginHandle = handle;
                    foundMajorVersion = handle.getPluginVersionMajor();
                    foundMinorVersion = handle.getPluginVersionMinor();
                } else if (foundMajorVersion == handle.getPluginVersionMajor()) {
                    if (foundMinorVersion < handle.getPluginVersionMinor()) {
                        pluginHandle = handle;
                        foundMajorVersion = handle.getPluginVersionMajor();
                        foundMinorVersion = handle.getPluginVersionMinor();
                    }
                }
            }
        }
        
        return pluginHandle;
    }

    public static <T> T getExtensionObjectByName(PluginManager manager, Logger logger, Class<T> interfaceType, String pluginName, String extensionName) {
        Extension<T> extension = getExtensionByName(manager, logger, interfaceType, pluginName, extensionName);
        return extension.getObject();
    }

    public static <T> T tryGetExtensionObjectByName(PluginManager manager, Logger logger, Class<T> interfaceType, String pluginName, String extensionName) {
        Extension<T> extension = tryGetExtensionByName(manager, logger, interfaceType, pluginName, extensionName);
        if (extension == null) {
            return null;
        } else {
            return extension.getObject();
        }
    }

    public static <T> Extension<T> getExtensionByName(PluginManager manager, Logger logger, Class<T> interfaceType, String pluginName, String extensionName) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (interfaceType == null) {
            throw new NullPointerException("interfaceType == null");
        }
        if (pluginName == null) {
            throw new NullPointerException("pluginName == null");
        }
        if (extensionName == null) {
            throw new NullPointerException("extensionName == null");
        }
        
        PluginHandle pluginHandle = PluginUtils.tryGetNewestPluginWithName(manager, logger, pluginName);
        
        if (pluginHandle == null) {
            logger.fatal("Could not load plugin '" + pluginName + "' for '" + interfaceType.getName() + "' extension '" + extensionName + "'");
            throw new RuntimeException();
        }
        
        final Plugin plugin;

        try {
            plugin = pluginHandle.getPlugin();
        } catch (PluginLoadException e) {
            logger.fatal("Error loading extensions of plugin " + pluginHandle.getPluginName() + ": " + e.getMessage(), e);
            throw new RuntimeException();
        }

        final Extension<T> extension = plugin.getExtensionByName(interfaceType, extensionName);
        
        if (extension == null) {
            logger.fatal("Could not load '" + interfaceType.getName() + "' extension '" + extensionName + "' in plugin '" + pluginName + "'");
            throw new RuntimeException();
        }
        
        return extension;
    }

    public static <T> Extension<T> tryGetExtensionByName(PluginManager manager, Logger logger, Class<T> interfaceType, String pluginName, String extensionName) {
        if (manager == null) {
            throw new NullPointerException("manager == null");
        }
        if (logger == null) {
            throw new NullPointerException("logger == null");
        }
        if (interfaceType == null) {
            throw new NullPointerException("interfaceType == null");
        }
        if (pluginName == null) {
            throw new NullPointerException("pluginName == null");
        }
        if (extensionName == null) {
            throw new NullPointerException("extensionName == null");
        }
        
        
        PluginHandle pluginHandle = PluginUtils.tryGetNewestPluginWithName(manager, logger, pluginName);
        
        if (pluginHandle == null) {
            return null;
        }
        
        final Plugin plugin;
        
        try {
            plugin = pluginHandle.getPlugin();
        } catch (PluginLoadException e) {
            logger.error("Error loading extensions of plugin " + pluginHandle.getPluginName() + ": " + e.getMessage(), e);
            return null;
        }
        
        final Extension<T> extension = plugin.getExtensionByName(interfaceType, extensionName);
        
        if (extension == null) {
            return null;
        }
        
        return extension;
    }
}
