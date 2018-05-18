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

import java.util.*;

import org.codecover.model.utils.*;

/**
 * An interface to be implemented by every plugin.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: AbstractPlugin.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AbstractPlugin implements Plugin {
    private final String name;
    private final String description;
    private final Map<Class<?>, Map<String, Extension<?>>> extensionMap;
    
    public AbstractPlugin(String name, String description, Extension<?>[] extensions) {
        this(name, description, toSet(extensions));
    }
    
    private static <T> Set<T> toSet(T[] ar) {
        final Set<T> result = new HashSet<T>();
        for (T element : ar) {
            result.add (element);
        }
        return Collections.unmodifiableSet(result);
    }
    
    public AbstractPlugin(String name, String description, Set<Extension<?>> extensions) {
        if (name == null) {
            throw new NullPointerException("name == null");
        }
        if (description == null) {
            throw new NullPointerException("description == null");
        }
        if (extensions == null) {
            throw new NullPointerException("extensions == null");
        }

        this.name = name;
        this.description = description;

        final Map<Class<?>, Map<String, Extension<?>>> myExtensionMap = new HashMap<Class<?>, Map<String, Extension<?>>>();
        for (Extension<?> extension : extensions) {
            Class<?> cl = extension.getInterface();

            Map<String, Extension<?>> m = myExtensionMap.get(cl);
            if (m == null) {
                m = new HashMap<String, Extension<?>>();
                myExtensionMap.put(cl, m);
            }
            
            m.put(extension.getName(), extension);
        }
        extensionMap = CollectionUtil.copyDeep(myExtensionMap);
    }
    
    public String getName() {
        return name;
    }
    
    public String getDescription() {
        return description;
    }
    
    // If java would have real generics, this would be easy.
    // Since java doesn't, we create a wrapper. (We also could cast it and
    // ignore the resulting warning, but this would be bad.)
    private <T> Extension<T> cast(final Class<T> interfaceType, final Extension<?> extension) {
        if (extension.getInterface() != interfaceType) {
            throw new IllegalArgumentException();
        }

        return new Extension<T>() {
            public String getName() {
                return extension.getName();
            }
    
            public T getObject() {
                Object o = extension.getObject();
                try {
                    return interfaceType.cast(o);
                } catch (ClassCastException e) {
                    throw new RuntimeException("An extension returns a interfaceType it does not implement.");
                }
            }
    
            public Class<T> getInterface() {
                return interfaceType;
            }
        };
    }
    
    public <T> Set<Extension<T>> getExtensions(Class<T> interfaceType) {
        final Map<String, Extension<?>> map = extensionMap.get(interfaceType);
        if (map == null) {
            return Collections.<Extension<T>>emptySet();
        }
        
        final Set<Extension<T>> result = new HashSet<Extension<T>>();
        for (Extension<?> extension : map.values()) {
            result.add(cast(interfaceType, extension));
        }
        return Collections.unmodifiableSet(result);
    }

    public <T> Extension<T> getExtensionByName(Class<T> interfaceType, String name) { 
        final Map<String, Extension<?>> map = extensionMap.get(interfaceType);
        if (map == null) {
            return null;
        }
        
        final Extension<?> result = map.get(name);
        if (result == null) {
            return null;
        } else {
            return cast(interfaceType, result);
        }
    }
}
