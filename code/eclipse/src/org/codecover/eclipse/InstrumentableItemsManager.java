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

package org.codecover.eclipse;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.codecover.eclipse.tscmanager.TSContainerManager;
import org.codecover.model.utils.CollectionUtil;
import org.codecover.model.utils.Logger;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.core.ICompilationUnit;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.IPackageFragment;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.ui.IMemento;
import org.eclipse.ui.XMLMemento;

/**
 * A {@link InstrumentableItemsManager} contains the selection of
 * {@link ICompilationUnit}s, that are to be used in the instrumentation
 * process. <br>
 * They are stored with their {@link IPackageFragment} parent, and the
 * {@link IJavaProject} they belong to.<br>
 * With the {@link #saveInstrumentableItems()} method, can the selection be
 * saved persistently in the metadata folder of the {@link CodeCoverPlugin}.
 * <br>
 * This selection is then loaded at the startup of the
 * {@link InstrumentableItemsManager}.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: InstrumentableItemsManager.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumentableItemsManager {
    private static final String TAG_INSTRUMENTABLE_ITEMS = "InstrumentableItems"; //$NON-NLS-1$

    private static final String TAG_I_JAVA_PROJECT = "IJavaProject"; //$NON-NLS-1$

    private static final String TAG_I_COMPILATION_UNIT = "ICompilationUnit"; //$NON-NLS-1$

    private static final String TAG_I_PACKAGE_FRAGMENT = "IPackageFragement"; //$NON-NLS-1$

    private static final String TAG_INFO = "Info"; //$NON-NLS-1$

    private final Logger logger;

    private final Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> markedJavaElements;

    private final Object lock = new Object();

    /**
     * Constructor
     * 
     * @param logger
     *            the logger to be used for logging
     */
    public InstrumentableItemsManager(Logger logger) {
        this.logger = logger;
        this.markedJavaElements = new HashMap<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>>();
        loadInstrumentableItems();
    }

    /**
     * Adds the given {@link IJavaElement} to the {@link TSContainerManager}
     * 
     * @param compilationUnit
     *            the to be added ICompilationUnit
     * @return indicates, whether the element was previously part of this
     *         {@link TSContainerManager}
     */
    public boolean addICompilationUnit(ICompilationUnit compilationUnit) {
        checkPreconditions(compilationUnit);

        IJavaProject project = compilationUnit.getJavaProject();
        IPackageFragment packageFragment = null;
        if (compilationUnit.getParent() instanceof IPackageFragment) {
            packageFragment = (IPackageFragment) compilationUnit.getParent();
        }
        if (project == null || packageFragment == null) {
            return false;
        }

        synchronized (this.lock) {
            Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> map = this.markedJavaElements;
            Map<IPackageFragment, Set<ICompilationUnit>> subMap = map
                    .get(project);
            if (subMap == null) {
                subMap = new HashMap<IPackageFragment, Set<ICompilationUnit>>();
                map.put(project, subMap);
            }

            Set<ICompilationUnit> compilationUnits = subMap
                    .get(packageFragment);
            if (compilationUnits == null) {
                compilationUnits = new HashSet<ICompilationUnit>();
                subMap.put(packageFragment, compilationUnits);
            }

            boolean result = compilationUnits.add(compilationUnit);

            return result;
        }
    }

    /**
     * Removes the given {@link IJavaElement} from the
     * {@link TSContainerManager}
     * 
     * @param compilationUnit
     *            the to be removed ICompilationUnit
     * @return indicates, whether the element was previously part of this
     *         {@link TSContainerManager}
     */
    public boolean removeICompilationUnit(ICompilationUnit compilationUnit) {
        checkPreconditions(compilationUnit);

        IJavaProject project = compilationUnit.getJavaProject();
        IPackageFragment packageFragment = null;
        if (compilationUnit.getParent() instanceof IPackageFragment) {
            packageFragment = (IPackageFragment) compilationUnit.getParent();
        }
        if (project == null || packageFragment == null) {
            return false;
        }

        synchronized (this.lock) {
            Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> map = this.markedJavaElements;
            Map<IPackageFragment, Set<ICompilationUnit>> subMap = map
                    .get(project);
            if (subMap == null) {
                return false;
            }

            Set<ICompilationUnit> compilationUnits = subMap
                    .get(packageFragment);
            if (compilationUnits == null) {
                return false;
            }

            boolean result = compilationUnits.remove(compilationUnit);

            return result;
        }
    }

    /**
     * Gets, whether or not the given {@link IJavaElement} is part of this
     * {@link TSContainerManager}
     * 
     * @param compilationUnit
     *            the to be confirmed ICompilationUnit
     * @return true &rarr; the element was part of this
     *         {@link TSContainerManager}<br>
     *         false&rarr; the element was not part of this
     *         {@link TSContainerManager}
     */
    public boolean containsICompilationUnit(ICompilationUnit compilationUnit) {
        checkPreconditions(compilationUnit);

        IJavaProject project = compilationUnit.getJavaProject();
        IPackageFragment packageFragment = null;
        if (compilationUnit.getParent() instanceof IPackageFragment) {
            packageFragment = (IPackageFragment) compilationUnit.getParent();
        }
        if (project == null || packageFragment == null) {
            return false;
        }

        synchronized (this.lock) {
            Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> map = this.markedJavaElements;
            Map<IPackageFragment, Set<ICompilationUnit>> subMap = map
                    .get(project);
            if (subMap == null) {
                return false;
            }

            Set<ICompilationUnit> compilationUnits = subMap
                    .get(packageFragment);
            if (compilationUnits == null) {
                return false;
            }

            return compilationUnits.contains(compilationUnit);
        }
    }

    /**
     * Indicates, whether the given {@link IPackageFragment} has any subItems
     * 
     * @param packageFragment
     *            the IPackageFragment, to be checked
     * @return true &rarr; the packageFragment had subentries in this
     *         {@link TSContainerManager}<br>
     *         false&rarr; the packageFragment had no subentries in this
     *         {@link TSContainerManager}
     */
    public boolean hasIPackageFragmentSubEntries(
            IPackageFragment packageFragment) {
        checkPreconditions(packageFragment);
        IJavaProject project = packageFragment.getJavaProject();

        if (project == null) {
            return false;
        }

        synchronized (this.lock) {
            Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> map = this.markedJavaElements;
            Map<IPackageFragment, Set<ICompilationUnit>> subMap = map
                    .get(project);
            if (subMap == null) {
                return false;
            }

            Set<ICompilationUnit> compilationUnits = subMap
                    .get(packageFragment);
            if (compilationUnits == null) {
                return false;
            }

            return !compilationUnits.isEmpty();
        }
    }

    private void checkPreconditions(ICompilationUnit compilationUnit) {
        if (compilationUnit == null) {
            throw new NullPointerException("compilationUnit == null"); //$NON-NLS-1$
        }
    }

    private void checkPreconditions(IPackageFragment packageFragment) {
        if (packageFragment == null) {
            throw new NullPointerException("packageFragment == null"); //$NON-NLS-1$
        }
    }

    private void loadInstrumentableItems() {
        FileReader reader = null;
        try {
            reader = new FileReader(getInstrumentableItemsFile());
            loadInstrumentableItems(XMLMemento.createReadRoot(reader));
        } catch (FileNotFoundException e) {
            // Ignored... no instrumentable items exist yet.
        } catch (Exception e) {
            // Log the exception and move on.
            this.logger
                    .error(
                            "An exception whilst loading the instrumentableItems occured", //$NON-NLS-1$
                            e);
        } finally {
            try {
                if (reader != null) {
                    reader.close();
                }
            } catch (IOException e) {
                this.logger.error("An IOException occured", e); //$NON-NLS-1$
            }
        }
    }

    private void loadInstrumentableItems(XMLMemento memento) {
        IMemento[] projectNodes = memento.getChildren(TAG_I_JAVA_PROJECT);

        for (IMemento projectNode : projectNodes) {
            String projectInfo = projectNode.getString(TAG_INFO);
            if (projectInfo == null) {
                continue;
            }

            IMemento[] packageNodes = projectNode
                    .getChildren(TAG_I_PACKAGE_FRAGMENT);

            for (IMemento packageNode : packageNodes) {
                String packageInfo = packageNode.getString(TAG_INFO);
                if (packageInfo == null) {
                    continue;
                }

                IMemento[] compilationUnitNodes = packageNode
                        .getChildren(TAG_I_COMPILATION_UNIT);

                for (IMemento compilationUnitNode : compilationUnitNodes) {
                    String compilationUnitInfo = compilationUnitNode
                            .getString(TAG_INFO);
                    if (compilationUnitInfo == null) {
                        continue;
                    }

                    ICompilationUnit compilationUnit = getICompilationUnit(getIPathFromString(compilationUnitInfo));
                    if (compilationUnit == null) {
                        continue;
                    }

                    addICompilationUnit(compilationUnit);
                }
            }
        }
    }

    /**
     * Checks for every project in {@link #markedJavaElements}, if any element
     * doesn't exist (was deleted) and removes unneccessary entries from the
     * map.
     */
    private void reconcileElementMap() {
        Set<IJavaProject> projectsToCheck = CollectionUtil
                .copy(this.markedJavaElements.keySet());

        for (IJavaProject javaProject : projectsToCheck) {
            Map<IPackageFragment, Set<ICompilationUnit>> map;
            map = this.markedJavaElements.get(javaProject);

            if (map == null || map.isEmpty()) {
                // We remove the entry, if the subMap was empty
                this.markedJavaElements.remove(javaProject);
                continue;
            }

            Set<IPackageFragment> packageFragments = CollectionUtil.copy(map
                    .keySet());
            for (IPackageFragment packageFragment : packageFragments) {
                Set<ICompilationUnit> subEntries = map.get(packageFragment);

                if (!packageFragment.exists()) {
                    // Remove all those IPackageFragment, that no longer exist
                    map.remove(packageFragment);
                    continue;
                }

                Set<ICompilationUnit> compilationUnits = CollectionUtil
                        .copy(subEntries);
                for (ICompilationUnit compilationUnit : compilationUnits) {

                    if (!compilationUnit.exists()) {
                        // Remove all those ICompilationUnits, that no
                        // longer exist
                        subEntries.remove(compilationUnit);
                        continue;
                    }
                }
            }
        }
    }

    /**
     * Gets the {@link Map} with the selected {@link ICompilationUnit}s per
     * {@link IJavaProject} and {@link IPackageFragment}
     * 
     * @return the {@link Map}
     */
    public Map<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> getMarkedJavaElements() {
        reconcileElementMap();
        synchronized (this.lock) {
            /*
             * XXX While this copies most of the map, the sets are the same,
             * maybe we should improve this some day.
             */
            return CollectionUtil.copyDeep(this.markedJavaElements);
        }
    }

    /**
     * Saves the data of this {@link InstrumentableItemsManager} persistently.
     */
    public void saveInstrumentableItems() {
        XMLMemento memento = XMLMemento
                .createWriteRoot(TAG_INSTRUMENTABLE_ITEMS);
        saveInstrumentableItems(memento);
        FileWriter writer = null;
        try {
            writer = new FileWriter(getInstrumentableItemsFile());
            memento.save(writer);
        } catch (IOException e) {
            this.logger.error("An IOException occured", e); //$NON-NLS-1$
        } finally {
            try {
                if (writer != null) {
                    writer.close();
                }
            } catch (IOException e) {
                this.logger.error("An IOException occured", e); //$NON-NLS-1$
            }
        }
    }

    private void saveInstrumentableItems(XMLMemento memento) {
        for (Map.Entry<IJavaProject, Map<IPackageFragment, Set<ICompilationUnit>>> entry : this.markedJavaElements
                .entrySet()) {
            Map<IPackageFragment, Set<ICompilationUnit>> subMap = entry
                    .getValue();
            IJavaProject javaProject = entry.getKey();

            // If the map is empty, we don't need to create a project node.
            // If the project doesn't exists, we don't create a node.
            if (subMap.isEmpty() || !javaProject.exists()) {
                continue;
            }

            IMemento projectNode = memento.createChild(TAG_I_JAVA_PROJECT);
            projectNode.putString(TAG_INFO, getIJavaElementInfo(javaProject));

            for (Map.Entry<IPackageFragment, Set<ICompilationUnit>> subEntry : subMap
                    .entrySet()) {
                Set<ICompilationUnit> set = subEntry.getValue();
                IPackageFragment packageFragment = subEntry.getKey();

                // If the set is empty, we don't need to create a package node.
                // If the package doesn't exists, we don't create a node.
                if (set.isEmpty() || !packageFragment.exists()) {
                    continue;
                }

                IMemento packageNode = projectNode
                        .createChild(TAG_I_PACKAGE_FRAGMENT);
                packageNode.putString(TAG_INFO,
                        getIJavaElementInfo(packageFragment));

                for (ICompilationUnit compilationUnit : set) {

                    // If the compilationUnit doesn't exists, we don't create a
                    // node.
                    if (!compilationUnit.exists()) {
                        continue;
                    }

                    IMemento compilationUnitNode = packageNode
                            .createChild(TAG_I_COMPILATION_UNIT);
                    compilationUnitNode.putString(TAG_INFO,
                            getIJavaElementInfo(compilationUnit));
                }
            }
        }
    }

    private String getIJavaElementInfo(IJavaElement javaElement) {
        try {
            return javaElement.getUnderlyingResource().getFullPath()
                    .toPortableString();
        } catch (JavaModelException e) {
            this.logger.error("A JavaModelException occured", e); //$NON-NLS-1$
            return null;
        }
    }

    private IPath getIPathFromString(String location) {
        if (location == null) {
            throw new NullPointerException("location == null"); //$NON-NLS-1$
        }

        return Path.fromPortableString(location);
    }

    private ICompilationUnit getICompilationUnit(IPath path) {
        IJavaElement javaElement = getIJavaElement(path);
        if (javaElement != null && javaElement instanceof ICompilationUnit) {
            return (ICompilationUnit) javaElement;
        }
        return null;
    }

    private IJavaElement getIJavaElement(IPath path) {
        IResource resource = ResourcesPlugin.getWorkspace().getRoot()
                .findMember(path);
        if (resource == null) {
            return null;
        }

        IJavaElement javaElement = JavaCore.create(resource);
        if (javaElement == null) {
            return null;
        }
        return javaElement;
    }

    /**
     * Checks, if this {@link InstrumentableItemsManager} contains a
     * {@link ICompilationUnit} with the given {@link IPath}
     * 
     * @param path
     *            the {@link IPath}
     * @return true &rarr; an {@link ICompilationUnit} with the given
     *         {@link IPath} exists and is part of this
     *         {@link InstrumentableItemsManager} <br>
     *         false &rarr; an {@link ICompilationUnit} with the given
     *         {@link IPath} does not exist or is not part of this
     *         {@link InstrumentableItemsManager}
     */
    public boolean containsIPath(IPath path) {
        ICompilationUnit compilationUnit = getICompilationUnit(path);

        if (compilationUnit == null) {
            return false;
        }

        return containsICompilationUnit(compilationUnit);
    }

    private File getInstrumentableItemsFile() {
        return CodeCoverPlugin.getDefault().getStateLocation().append(
                "instrumentableItems.xml").toFile(); //$NON-NLS-1$
    }
}
