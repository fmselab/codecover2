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
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.extensions.PluginManager;
import org.codecover.model.extensions.PluginUtils;
import org.codecover.model.utils.Logger;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: Context.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class Context {
    private final Project project;
    private final Logger logger;
    private final Map<String, TestSessionContainer> testSessionContainers = new TreeMap<String, TestSessionContainer>();
    private final MASTBuilder mastBuilder;
    private final PluginManager pluginManager;

    private static final PluginUtils.ClassLoaderHandler antClassLoaderHandler = new PluginUtils.ClassLoaderHandler() {
            public List<File> handleClassLoader(Logger logger, ClassLoader classLoader) {
                if (!(classLoader instanceof AntClassLoader)) {
                    return null;
                }

                final AntClassLoader antClassLoader = (AntClassLoader) classLoader;

                final List<File> resultList = new ArrayList<File>();
            
                // Hmm. ant is trying to make it difficult...
                final String classpath = antClassLoader.getClasspath();
                int pos = 0;
                for (;;) {
                    final int newpos = classpath.indexOf(File.pathSeparator, pos);
                    final String str = newpos == -1 ? classpath.substring(pos) : classpath.substring(pos, newpos);
                    
                    if (str.length() > 0) {
                        resultList.add(new File(str));
                    }
                
                    if (newpos == -1) {
                        break;
                    }
                    pos = newpos + 1;
                }
                
                return resultList;
            }
        };

    /**
     * Constructor
     * @param project the given {@link Project}.
     * @param logger the {@link Logger} to use.
     */
    public Context(Project project, Logger logger) {
        this.project = project;
        this.logger = logger;
        this.mastBuilder = new MASTBuilder(logger);
        this.pluginManager = PluginManager.create();
        
        File pluginDir = PluginUtils.getPluginDirectory(logger, PluginUtils.getPathOfClass(logger, Context.class, Collections.singletonList(antClassLoaderHandler)));
        if (pluginDir != null) {
            PluginUtils.loadPluginsFromDirectory(this.pluginManager, logger, pluginDir);
        }
    }
    
    /**
     * Gets the {@link Project}
     * @return the project
     */
    public Project getProject() {
        return this.project;
    }
    
    /**Gets the {@link Logger}
     * @return the logger.
     */
    public Logger getLogger() {
        return this.logger;
    }
    
    /**
     * Gets the {@link PluginManager}
     * 
     * @return the pluginManager.
     */
    public PluginManager getPluginManager() {
        return this.pluginManager;
    }
    
    /**
     * Stores the given {@link TestSessionContainer} under the given id.
     * 
     * @param id
     *                the id to be used in storing the
     *                {@link TestSessionContainer}
     * @param container
     *                the {@link TestSessionContainer} to store.
     */
    public void setTestSessionContainer(String id, TestSessionContainer container) {
        if (id == null) {
            throw new NullPointerException("id == null");
        }
        
        if (container == null) {
            throw new NullPointerException("container == null");
        }
        
        this.testSessionContainers.put(id, container);
    }
    
    /**
     * Gets the {@link TestSessionContainer}, that was stored with the given
     * id.
     * 
     * @param id
     *                the id to be used in retrieving the
     *                {@link TestSessionContainer}.
     * @return the retrieved {@link TestSessionContainer}.
     */
    public TestSessionContainer getTestSessionContainer(String id) {
        if (id == null) {
            throw new NullPointerException("id == null");
        }
        
        final TestSessionContainer container = this.testSessionContainers.get(id);
        
        if (container == null) {
            throw new BuildException("There is no test session container with the id '" + id + "'");
        }
        
        return container;
    }
    
    /**
     * Gets the {@link MASTBuilder}
     * @return the mastBuilder
     */
    public MASTBuilder getMASTBuilder() {
        return this.mastBuilder;
    }
}
