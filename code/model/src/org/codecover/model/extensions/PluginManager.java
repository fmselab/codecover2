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
import org.w3c.dom.*;
import org.xml.sax.*;
import org.xml.sax.helpers.*;
import javax.xml.parsers.*;

import org.codecover.model.utils.*;

/**
 * A plugin manager.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: PluginManager.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class PluginManager {
    public static interface PluginNameResolver {
        public File resolve(String name, int majorVersion) throws PluginLoadException;
    }
    
    private final Object lock = new Object();
    
    private static class LoadedPlugin {
        public volatile PluginHandle handle = null;
        public Throwable error = null;
    }
    
    private final Map<String, TreeMap<Integer, LoadedPlugin>> loadedPlugins = new TreeMap<String, TreeMap<Integer, LoadedPlugin>>();
    
    private PluginManager() {
    }
    
    public PluginHandle getPluginHandle(PluginNameResolver resolver, String name, int major) throws PluginLoadException {
        return getPluginHandle(resolver, name, major, new TreeSet<String>());
    }
    
    private PluginHandle getPluginHandle(final PluginNameResolver resolver, String name, int major, TreeSet<String> loadingPlugins) throws PluginLoadException {
        if (resolver == null) {
            throw new NullPointerException("resolver == null");
        }
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        if (major < 0) {
            throw new IllegalArgumentException("major < 0");
        }
        if (loadingPlugins == null) {
            throw new IllegalArgumentException("loadingPlugins == null");
        }

        if (loadingPlugins.contains(name)) {
            throw new PluginLoadException("Cyclic dependencies while loading plugin '" + name + "'");
        }
        
        LoadedPlugin loadedPlugin = null;
        boolean removeMyLoadingPlugin = false;
        
        final LoadedPlugin myLoadingPlugin = new LoadedPlugin();
        synchronized(myLoadingPlugin) {
            try {
                synchronized(lock) {
                    TreeMap<Integer, LoadedPlugin> map = loadedPlugins.get(name);
                    if (map != null) {
                        loadedPlugin = map.get(major);
                    }
                    if (loadedPlugin == null) {
                        // The plugin is not loaded and no one is loading it.
                        // We have to do it ourselves.
                        if (map == null) {
                            map = new TreeMap<Integer, LoadedPlugin>();
                            loadedPlugins.put(name, map);
                        }
                        removeMyLoadingPlugin = true;
                        map.put(major, myLoadingPlugin);
                    }
                }
                if (loadedPlugin == null) {
                    try {
                        loadingPlugins.add(name);
                        try {
                            final File fileToLoad = resolver.resolve(name, major);
                            if (fileToLoad == null) {
                                throw new IllegalArgumentException("PluginNameResolver.resolve returned null");
                            }
                            final PluginHandle handle = loadPlugin(resolver, fileToLoad, loadingPlugins);
                            if (!handle.getPluginName().equals(name)) {
                                throw new PluginLoadException("Loaded plugin has name '" + handle.getPluginName() + "', expected '" + name + "'");
                            }
                            if (handle.getPluginVersionMajor() != major) {
                                throw new PluginLoadException("Loaded plugin '" + name + "' has major version " + handle.getPluginVersionMajor() + ", expected " + major);
                            }
                            myLoadingPlugin.handle = handle;
                        } finally {
                            loadingPlugins.remove(name);
                        }
                    } catch (Throwable t) {
                        // Hmm. We're catching Throwable here because we want
                        // to know which error occured. We cannot simply
                        // rethrow the error because Java has no support for
                        // this and has its wonderful "checked Exceptions"
                        // We try to rethrow at least subclasses of Error.
                        myLoadingPlugin.error = t;
                        if (t instanceof Error) {
                            throw (Error)t;
                        }
                        throw new PluginLoadException("An error occured when loading the plugin " + name + ": " + t, t);
                    }
                    removeMyLoadingPlugin = false;
                    return myLoadingPlugin.handle;
                }
            } finally {
                if (removeMyLoadingPlugin) {
                    synchronized(lock) {
                        TreeMap<Integer, LoadedPlugin> map = loadedPlugins.get(name);
                        if (map != null) { // should always be
                            LoadedPlugin pl = map.get(major);
                            if (pl == myLoadingPlugin) { // should always be
                                map.remove(major);
                            }
                        }
                    }
                }
            }
        }
        
        if (loadedPlugin == null) {
            throw new RuntimeException();
        }
        
        synchronized(loadedPlugin) {
            if (loadedPlugin.handle == null) {
                throw new PluginLoadException("An error occured when loading the plugin " + name + ": " + loadedPlugin.error, loadedPlugin.error);
            }
            return loadedPlugin.handle;
        }
    }
    
    public PluginHandle loadPlugin(PluginNameResolver resolver, File jarFile) throws PluginLoadException {
        return loadPlugin(resolver, jarFile, new TreeSet<String>());
    }

    private PluginHandle loadPlugin(final PluginNameResolver resolver, File jarFile, final TreeSet<String> loadingPlugins) throws PluginLoadException {
        if (resolver == null) {
            throw new NullPointerException("resolver == null");
        }
        return PluginHandle.load(jarFile, new PluginHandle.PluginLoadResolver() {
                public PluginHandle resolve(String name, int majorVersion, int minimalMinorVersion) throws PluginLoadException {
                    try {
                        return getPluginHandle(resolver, name, majorVersion, loadingPlugins);
                    } catch (PluginLoadException e) {
                        throw new PluginDependencyLoadException(name, majorVersion, e);
                    }
                }
            });
    }
    
    public boolean addPlugin(PluginHandle handle) {
        if (handle == null) {
            throw new NullPointerException("handle == null");
        }
        
        final String name = handle.getPluginName();
        final int major = handle.getPluginVersionMajor();
        
        final LoadedPlugin loadedPlugin = new LoadedPlugin();
        loadedPlugin.handle = handle;
        
        synchronized (lock) {
            TreeMap<Integer, LoadedPlugin> map = loadedPlugins.get(name);
            if (map == null) {
                map = new TreeMap<Integer, LoadedPlugin>();
                loadedPlugins.put(name, map);
            }
            if (map.get(major) == null) {
                map.put(major, loadedPlugin);
                return true;
            } else {
                return false;
            }
        }
    }
    
    public Set<PluginHandle> getPluginHandles() {
        final Set<PluginHandle> result = new HashSet<PluginHandle>();

        synchronized (lock) {
            for (TreeMap<Integer, LoadedPlugin> map : loadedPlugins.values()) {
                for (LoadedPlugin plugin : map.values()) {
                    final PluginHandle handle = plugin.handle;
                    if (handle != null) {
                        result.add(handle);
                    }
                }
            }
        }
        
        return Collections.unmodifiableSet(result);
    }
    
    public static PluginManager create() {
        return create(true);
    }

    public static PluginManager create(boolean addCodecoverPlugin) {
        final PluginManager manager = new PluginManager();
        if (addCodecoverPlugin) {
            manager.addPlugin(PluginHandle.codeCoverPluginHandle);
        }
        return manager;
    }
}
