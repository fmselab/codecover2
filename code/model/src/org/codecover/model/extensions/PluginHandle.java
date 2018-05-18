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

import org.codecover.*;
import org.codecover.model.utils.*;
import org.codecover.model.utils.criteria.*;

/**
 * A reference to a plugin.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: PluginHandle.java 71 2010-04-14 18:28:46Z schmidberger $)
 */
public class PluginHandle {
    public static interface PluginLoadResolver {
        public PluginHandle resolve(String name, int majorVersion, int minimalMinorVersion) throws PluginLoadException;
    }
    
    public static interface ClassNameResolver {
        public Class<?> resolve(String name) throws ClassNotFoundException;
    }
    
    private class DependencyLoader extends ClassLoader {
        public PluginClassLoader callingClassLoader = null;
        
        public DependencyLoader() {
            super (null);
        }
        
        protected Class<?> findClass(String name) throws ClassNotFoundException {
            // try plugin itself before dependencies
            try {
                return callingClassLoader.loadNonRecursive(name);
            } catch (ClassNotFoundException e) {
                // continue
            }

            for (PluginHandle plugin : dependencies) {
                try {
                    return plugin.getClassNameResolver().resolve(name);
                } catch (ClassNotFoundException e) {
                    // continue
                }
            }
            
            throw new ClassNotFoundException("The plugin " + getPluginName() + " version " + getPluginVersion() + " could not find the class " + name + " in any of its dependencies");
        }
    }
        
    private class PluginClassLoader extends URLClassLoader {
        public PluginClassLoader(URL[] urls, ClassLoader parent) {
            super(urls, parent);
        }
        
        public Class<?> loadNonRecursive(String name) throws ClassNotFoundException {
            return findClass(name);
        }
        
        public URL getResourceNonRecursive(String name) {
            return findResource(name);
        }
    }
    
    private final String pluginName;
    private final String pluginClassName;
    private final int pluginVersionMajor;
    private final int pluginVersionMinor;
    private final ClassNameResolver classNameResolver;
    
    // These two are not really needed, only for returning debug information
    // Should maybe removed.
    private final File jarFile;
    private final List<PluginHandle> dependencies;

    private final Object lock = new Object();

    private volatile Plugin plugin = null;
    
    private static class CodeCoverPlugin extends AbstractPlugin {
        // Hack to reference to extensions without having a static link to them
        // TODO: This is not very nice.
        private static class DynamicReferencedExtension extends AbstractExtension<Object> {
            public DynamicReferencedExtension(String interfaceType, String className) {
                super (getclass(interfaceType), className);
            }
        
            private static Class<Object> getclass(String name) {
                try { 
                    return (Class<Object>) Class.forName(name);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        
            public Object getObject() {
                try { 
                    final Object o = Class.forName(getName()).getMethod("getInstance").invoke(null);
                    return getInterface().cast(o);
                } catch (Exception e) {
                    throw new RuntimeException(e);
                }
            }
        }
        
        private static Set<Extension<?>> myGetExtensions() {
            final Extension<?>[] ar = (new Extension<?>[] {
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.StatementCoverage") {
                        public Criterion getObject() {
                            return StatementCoverage.getInstance();
                        }
                    },
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.BranchCoverage") {
                        public Criterion getObject() {
                            return BranchCoverage.getInstance();
                        }
                    },
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.LoopCoverage") {
                        public Criterion getObject() {
                            return LoopCoverage.getInstance();
                        }
                    },
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.ConditionCoverage") {
                        public Criterion getObject() {
                            return ConditionCoverage.getInstance();
                        }
                    },
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.SynchronizedStatementCoverage") {
                        public Criterion getObject() {
                            return SynchronizedStatementCoverage.getInstance();
                        }
                    },
                    new AbstractExtension<Criterion>(Criterion.class, "org.codecover.model.utils.criteria.QMOCoverage") {
                        public Criterion getObject() {
                            return QMOCoverage.getInstance();
                        }
                    },
                });
            final Set<Extension<?>> result = new HashSet<Extension<?>>();
            for (Extension<?> element : ar) {
                result.add (element);
            }

            for (String s : new String[] {"org.codecover.metrics.coverage.StatementCoverage", 
            		"org.codecover.metrics.coverage.TermCoverage", 
            		"org.codecover.metrics.coverage.LoopCoverage", 
            		"org.codecover.metrics.coverage.BranchCoverage", 
            		"org.codecover.metrics.coverage.QMOCoverage", 
            		"org.codecover.metrics.coverage.SynchronizedCoverage", 
            		"org.codecover.metrics.correlation.StatementCorrelation", 
            		"org.codecover.metrics.correlation.ConditionCorrelation", 
            		"org.codecover.metrics.correlation.LoopCorrelation", 
            		"org.codecover.metrics.correlation.BranchCorrelation"}) {
                try {
                    result.add(new DynamicReferencedExtension("org.codecover.metrics.Metric", s));
                } catch (Exception e) {
                    // ignore
                }
            }
            
            return result;
        }
        
        public CodeCoverPlugin() {
            super("CodeCover", "", myGetExtensions());
        }
    }
    
    private PluginHandle(String pluginName, int pluginVersionMajor, int pluginVersionMinor, Plugin plugin, ClassNameResolver classNameResolver) {
        if (pluginName == null) {
            throw new NullPointerException("pluginName == null");
        }
        if (pluginVersionMajor < 0) {
            throw new IllegalArgumentException("pluginVersionMajor < 0");
        }
        if (pluginVersionMinor < 0) {
            throw new IllegalArgumentException("pluginVersionMinor < 0");
        }
        if (plugin == null) {
            throw new NullPointerException("plugin == null");
        }
        if (classNameResolver == null) {
            throw new NullPointerException("classNameResolver == null");
        }
        
        this.pluginName = pluginName;
        this.pluginVersionMajor = pluginVersionMajor;
        this.pluginVersionMinor = pluginVersionMinor;
        this.plugin = plugin;
        this.pluginClassName = plugin.getClass().getName();
        jarFile = null;
        dependencies = Collections.<PluginHandle>emptyList();
        this.classNameResolver = classNameResolver;
    }

    public static PluginHandle createPluginHandle(String pluginName, int pluginVersionMajor, int pluginVersionMinor, Plugin plugin, ClassNameResolver classNameResolver) {
        return new PluginHandle(pluginName, pluginVersionMajor, pluginVersionMinor, plugin, classNameResolver);
    }
    
    public static PluginHandle createPluginHandle(String pluginName, int pluginVersionMajor, int pluginVersionMinor, final Plugin plugin) {
        return new PluginHandle(pluginName, pluginVersionMajor, pluginVersionMinor, plugin, new ClassNameResolver() {
                final ClassLoader loader = plugin.getClass().getClassLoader();
                public Class resolve(String name) throws ClassNotFoundException {
                    return loader.loadClass(name);
                }
            });
    }
    
    public static final PluginHandle codeCoverPluginHandle = createPluginHandle("org.codecover", CodeCoverInfo.pluginVersionMajor, CodeCoverInfo.pluginVersionMinor, new CodeCoverPlugin());
    
    private PluginHandle(File jarFile, PluginLoadResolver resolver) throws PluginLoadException {
        if (jarFile == null) {
            throw new NullPointerException("jarFile == null");
        }
        if (resolver == null) {
            throw new NullPointerException("resolver == null");
        }

        this.jarFile = jarFile;
        
        final URL url;
        try {
            url = jarFile.toURI().toURL();
        } catch (MalformedURLException e) {
            // should not happen because File.toURI() hopefully won't
            // create anything malformed
            throw new RuntimeException(e);
        }
        final DependencyLoader depLoader = new DependencyLoader();
        final PluginClassLoader loader = new PluginClassLoader(new URL[] {url}, depLoader);
        depLoader.callingClassLoader = loader;
        
        final URL xmlUrl = loader.getResourceNonRecursive("codecover-plugin.xml");
        if (xmlUrl == null) {
            throw new NoCodecoverPluginException(jarFile);
        }
        
        final Document document;
        try {
            final URLConnection connection = xmlUrl.openConnection();
            
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            factory.setNamespaceAware(true);
            final DocumentBuilder builder = factory.newDocumentBuilder();
            document = builder.parse(connection.getInputStream());
        } catch (IOException e) {
            throw new PluginLoadException("An IOException occured while loeader the plugin information", e);
        } catch (ParserConfigurationException e) {
            // should not happen
            throw new RuntimeException(e);
        } catch (SAXException e) {
            throw new PluginLoadException("codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' contains invalid XML", e);
        }
        
        final Element root = document.getDocumentElement();
        
        if (!"http://www.codecover.org/xml/plugin-descriptor-1.0".equals(root.getNamespaceURI())) {
            throw new PluginLoadException("The root element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has the namespace uri '" + root.getNamespaceURI() + "' instead of 'http://www.codecover.org/xml/plugin-descriptor-1.0'");
        }

        if (!"plugin".equals(root.getLocalName())) {
            throw new PluginLoadException("The root element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has the name '" + root.getLocalName() + "' instead of 'plugin'");
        }
        
        if (!root.hasAttribute("name")) {
            throw new PluginLoadException("The root element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has no 'name' attribute");
        }
        pluginName = root.getAttribute("name");
        
        if (!root.hasAttribute("version")) {
            throw new PluginLoadException("The root element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has no 'version' attribute");
        }
        pluginVersionMajor = parseVersion(root.getAttribute("version"), false);
        pluginVersionMinor = parseVersion(root.getAttribute("version"), true);
        
        if (!root.hasAttribute("class")) {
            throw new PluginLoadException("The root element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has no 'class' attribute");
        }
        pluginClassName = root.getAttribute("class");
        
        final List<PluginHandle> deps = new ArrayList<PluginHandle>();
        for (final Element element : getChildElements(root)) {
            if (!"http://www.codecover.org/xml/plugin-descriptor-1.0".equals(element.getNamespaceURI()) || !"depends".equals(element.getLocalName())) {
                continue;
            }
            
            if (!element.hasAttribute("name")) {
                throw new PluginLoadException("A depends element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has no 'name' attribute");
            }
            final String depName = element.getAttribute("name");
        
            if (!element.hasAttribute("version")) {
                throw new PluginLoadException("A depends element of codecover-plugin.xml for the plugin in '" + jarFile.getAbsolutePath() + "' has no 'version' attribute");
            }
            final int depVersionMajor = parseVersion(element.getAttribute("version"), false);
            final int depVersionMinor = parseVersion(element.getAttribute("version"), true);
            
            final PluginHandle dep = resolver.resolve(depName, depVersionMajor, depVersionMinor);
            
            if (!dep.getPluginName().equals(depName)) {
                throw new PluginLoadException("Got plugin " + dep.getPluginName() + " when plugin " + depName + " was needed.");
            }
            
            if (dep.getPluginVersionMajor() != depVersionMajor) {
                throw new PluginLoadException("Plugin " + depName + " has wrong major version (" + dep.getPluginVersionMajor() + " instead of " + depVersionMajor + ")");
            }

            if (dep.getPluginVersionMinor() < depVersionMinor) {
                throw new PluginLoadException("Plugin " + depName + " has too low minor version (" + dep.getPluginVersionMinor() + ", but at least " + depVersionMinor + " is needed)");
            }
            
            deps.add(dep);
        }
        dependencies = CollectionUtil.copy(deps);
        classNameResolver = new ClassNameResolver() {
                public Class resolve(String name) throws ClassNotFoundException {
                    return loader.loadNonRecursive(name);
                }
            };
    }
    
    private static int parseVersion(String str, boolean returnMinor) throws PluginLoadException {
        String[] parts = str.split("\\.");
        if (parts.length != 2) {
            throw new PluginLoadException("Version number '" + str + " does not contain exactly one point.");
        }
        
        if (!returnMinor) {
            final int number;
            try {
                number = Integer.parseInt(parts[0]);
            } catch (NumberFormatException e) {
                throw new PluginLoadException("Version number '" + str + " contains an unparseable major number: '" + parts[0] + "'");
            }
            if (number < 0) {
                throw new PluginLoadException("Version number '" + str + " contains an negative major number: '" + parts[0] + "'");
            }
            return number;
        } else {
            final int number;
            try {
                number = Integer.parseInt(parts[1]);
            } catch (NumberFormatException e) {
                throw new PluginLoadException("Version number '" + str + " contains an unparseable minor number: '" + parts[1] + "'");
            }
            if (number < 0) {
                throw new PluginLoadException("Version number '" + str + " contains an negative minor number: '" + parts[1] + "'");
            }
            return number;
        }
    }
    
    public Plugin getPlugin() throws PluginLoadException {
        Plugin myPlugin = plugin;
        if (myPlugin != null) {
            return myPlugin;
        }
        
        synchronized (lock) {
            myPlugin = plugin;
            if (myPlugin != null) {
                return myPlugin;
            }
            
            final Class<?> pluginClass;
            try {
                pluginClass = getClassNameResolver().resolve(pluginClassName);
            } catch (ClassNotFoundException e) {
                throw new PluginLoadException("The plugin class name " + pluginClassName + " of the plugin " + getPluginName() + " version " + getPluginVersion() + " cannot be found in its jar file.", e);
            }
            final Class<? extends Plugin> castedPluginClass;
            try {
                castedPluginClass = pluginClass.asSubclass(Plugin.class);
            } catch (ClassCastException e) {
                throw new PluginLoadException("The plugin class " + pluginClassName + " of the plugin " + getPluginName() + " version " + getPluginVersion() + " does not implement the Plugin interface.", e);
            }
            try {
                myPlugin = castedPluginClass.newInstance();
            } catch (Exception e) {
                throw new PluginLoadException("The instance for the plugin class " + pluginClassName + " of the plugin " + getPluginName() + " version " + getPluginVersion() + " could not be created.", e);
            }
            plugin = myPlugin;
            return myPlugin;
        }
    }
    
    /*
    private PluginClassLoader getLoader() {
        return loader;
    }
    */
    
    public ClassNameResolver getClassNameResolver() {
        return classNameResolver;
    }

    public String getPluginName() {
        return pluginName;
    }
    
    public int getPluginVersionMajor() {
        return pluginVersionMajor;
    }
    
    public int getPluginVersionMinor() {
        return pluginVersionMinor;
    }
    
    public String getPluginVersion() {
        return getPluginVersionMajor() + "." + getPluginVersionMinor();
    }
    
    public static PluginHandle load(File jarFile, PluginLoadResolver resolver) throws PluginLoadException {
        return new PluginHandle(jarFile, resolver);
    }

    /**
     * Returns the child elements of the given element.
     * 
     * @param element
     *            the element.
     * 
     * @return the child elements.
     * 
     * @throws PluginLoadException
     *             if {@code element} contains anything but other elements,
     *             comments and whitespace
     */
    public static List<Element> getChildElements(Element element)
            throws PluginLoadException {
        final NodeList children = element.getChildNodes();
        final List<Element> list = new ArrayList<Element>();
        for (int i = 0; i < children.getLength(); i++) {
            Node node = children.item(i);
            switch (node.getNodeType()) {
            case Node.ELEMENT_NODE:
                list.add((Element) node);
                break;

            case Node.COMMENT_NODE:
                break;

            case Node.TEXT_NODE: // Ignore whitespace
                String text = node.getTextContent();
                if (text.trim().equals("")) {
                    break;
                }
            // fall through

            default:
                throw new PluginLoadException("Got a child node of type "
                        + node.getNodeType() + " in a element of type "
                        + element.getLocalName()
                        + " where only child elements where expected ", null);
            }
        }
        return Collections.unmodifiableList(list);
    }
}
