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

package org.codecover.instrumentation;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.regex.Pattern;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevel;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.StatementSequence;

/**
 * A container to create {@link HierarchyLevel}s.<br>
 * <br>
 * The problem is, that the {@link HierarchyLevel}s of the MAST are immutable.
 * This means if a class, created in a source file, wants to be inserted in a
 * super {@link HierarchyLevel}, the super {@link HierarchyLevel} cannot take
 * up other child {@link HierarchyLevel}s later. In the consequence this
 * container is created.<br>
 * A container has a name and a {@link HierarchyLevelType}&mdash;equal to
 * {@link HierarchyLevel}. A container contains on the one hand
 * {@link HierarchyLevel}s ({@link #hierarchyLevelList})&mdash;e.g classes.
 * On the other hand it contains {@link HierarchyLevelContainer}, that are
 * children too. But these can take up {@link HierarchyLevel}s after creation.<br>
 * {@link HierarchyLevel}s are added by using
 * {@link #addHierarchyLevels(Collection, LinkedList)} or an equal method. The
 * stated LinkedList defines, in which sub container the {@link HierarchyLevel}s
 * of the container are inserted.<br>
 * Finally {@link #transformToHierarchyLevel(MASTBuilder)} will transform this
 * representation of {@link HierarchyLevel}s in a equal representation using
 * just {@link HierarchyLevel}s.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: HierarchyLevelContainer.java 1 2007-12-12 17:37:26Z t-scheller $)<br>
 */
public class HierarchyLevelContainer {
    private List<HierarchyLevelContainer> children = new LinkedList<HierarchyLevelContainer>();

    private List<HierarchyLevel> hierarchyLevelList = new LinkedList<HierarchyLevel>();

    private String name;

    private HierarchyLevelType type;

    private HierarchyLevelType subtypes;

    /**
     * Constructs a {@link HierarchyLevelContainer}.
     *
     * @param name the name of the root type
     * @param type the root type
     * @param subtypes the type of the children
     */
    public HierarchyLevelContainer(String name, HierarchyLevelType type,
            HierarchyLevelType subtypes) {
        this.name = name;
        this.type = type;
        this.subtypes = subtypes;
    }

    /**
     * Adds a created {@link HierarchyLevel} to the root container.<br>
     * <br>
     * Same as
     * <code>addHierarchyLevel(newHierarchyLevel, &lt;emptyList&gt;)</code>
     * 
     * @param newHierarchyLevel
     *                The created {@link HierarchyLevel}.
     */
    public void addHierarchyLevelToRoot(HierarchyLevel newHierarchyLevel) {
        addHierarchyLevels(Collections.<HierarchyLevel>singletonList(newHierarchyLevel),
                new LinkedList<String>());
    }

    /**
     * Adds a Collection of created {@link HierarchyLevel}s to the root
     * container.<br>
     * <br>
     * Same as
     * <code>addHierarchyLevels(newHierarchyLevels, &lt;emptyList&gt;)</code>
     * 
     * @param newHierarchyLevels
     *                The Collection of created {@link HierarchyLevel}s.
     */
    public void addHierarchyLevelsToRoot(Collection<HierarchyLevel> newHierarchyLevels) {
        addHierarchyLevels(newHierarchyLevels, new LinkedList<String>());
    }

    /**
     * Adds a created {@link HierarchyLevel} to this container or a
     * sub container.
     * 
     * @param newHierarchyLevel
     *            The created {@link HierarchyLevel}.
     * @param packageNames
     *            The names of the target package hierarchical. Use
     *            {@link #packagePathToList(String, boolean)} for creation.
     */
    public void addHierarchyLevel(HierarchyLevel newHierarchyLevel,
            LinkedList<String> packageNames) {
        addHierarchyLevels(Collections.<HierarchyLevel>singletonList(newHierarchyLevel),
                packageNames);
    }

    /**
     * Adds a Collection of {@link HierarchyLevel}s to this container or a
     * sub container.<br>
     * <br>
     * If the List of packageNames is empty, the {@link HierarchyLevel}s are
     * inserted in this container. If not, the correct child container is
     * searched in {@link #children} or created. Then the new
     * {@link HierarchyLevel}s are handed over to this child container and so
     * on.
     * 
     * @param newHierarchyLevels
     *            The Collection of created {@link HierarchyLevel}s.
     * @param packageNames
     *            The names of the target package hierarchical. Use
     *            {@link #packagePathToList(String, boolean)} for creation.
     */
    public void addHierarchyLevels(Collection<HierarchyLevel> newHierarchyLevels,
            LinkedList<String> packageNames) {
        if (packageNames.isEmpty()) {
            // we have to insert it here
            this.hierarchyLevelList.addAll(newHierarchyLevels);
        } else {
            String topLevelName = packageNames.poll();
            HierarchyLevelContainer childToUse = null;
            for (HierarchyLevelContainer thisChildContainer : this.children) {
                if (thisChildContainer.name.equals(topLevelName)) {
                    childToUse = thisChildContainer;
                    break;
                }
            }
            if (childToUse == null) {
                childToUse = new HierarchyLevelContainer(topLevelName,
                        this.subtypes, this.subtypes);
                this.children.add(childToUse);
            }
            childToUse.addHierarchyLevels(newHierarchyLevels, packageNames);
        }
    }

    /**
     * Returns the package name and the internal name of the type.
     *
     * @return The package name and the internal name of the type.
     */
    @Override
    public String toString() {
        return this.name + " (" + this.type.getInternalName() + ")";
    }

    /**
     * The name of this {@link HierarchyLevelContainer}.
     * 
     * @return The name.
     */
    String getName() {
        return this.name;
    }

    /**
     * The {@link HierarchyLevelType} of this container.
     * 
     * @return The type.
     */
    HierarchyLevelType getType() {
        return this.type;
    }

    /**
     * The {@link HierarchyLevelType} of new child container.
     * 
     * @return The type used for new child containers.
     */
    HierarchyLevelType getSubtypes() {
        return this.subtypes;
    }

    /**
     * Transforms this {@link HierarchyLevelContainer} and its children to a
     * equal representation using {@link HierarchyLevel}s of the MAST.
     * 
     * @param builder
     *            The {@link MASTBuilder} used for
     *            {@link MASTBuilder#createHierarchyLevel(org.codecover.model.mast.LocationList, String, org.codecover.model.mast.LocationList, HierarchyLevelType, List, List)}
     * @return A {@link HierarchyLevel} with all the {@link #children} and
     *         {@link #hierarchyLevelList} as children.
     */
    HierarchyLevel transformToHierarchyLevel(MASTBuilder builder) {
        // we transform the children to HierarchyLevels
        List<HierarchyLevel> returnList = new LinkedList<HierarchyLevel>();
        for (HierarchyLevelContainer thisChild : this.children) {
            HierarchyLevel childHiLevels = thisChild.transformToHierarchyLevel(builder);
            // only if the child contains at least a grand child it is added
            if (!childHiLevels.getChildren().isEmpty()) {
                returnList.add(childHiLevels);
            }
        }

        returnList.addAll(this.hierarchyLevelList);

        return builder.createHierarchyLevel(
                builder.createEmptyLocationList(),
                this.name,
                builder.createEmptyLocationList(),
                this.type,
                returnList,
                Collections.<StatementSequence> emptyList());
    }

    /**
     * The String, that is used to concatenate package names.
     * 
     * @see #packagePathToList(String, boolean)
     */
    public static final String SEPARATOR = ".";

    /**
     * The regular expression String to split the fullPackageName of
     * {@link #packagePathToList(String, boolean)}. This is different to
     * {@link #SEPARATOR} cause a dot has a special meaning as an regular
     * Expression.
     * 
     * @see #packagePathToList(String, boolean)
     * @see Pattern
     */
    public static final String SEPARATOR_REG_EXP = "\\.";

    /**
     * Splits a given package name&mdash;e.g.
     * <code>org.codecover.instrumentation</code>&mdash;into a List of
     * hierarchical package names.<br>
     * <br>
     * The List contains the packages from the top package down to the given
     * package. Giving as package name an empty String, the List is empty.<br>
     * For the creation of the name of the fullPackagePath,
     * {@link #SEPARATOR_REG_EXP} and {@value #SEPARATOR} are used.
     * 
     * @param fullPackagePath
     *            The full name of the package as a String; e.g.
     *            <code>org.codecover.instrumentation</code>
     * @param fullPackageNames
     *            States whether or whether the names of the packages should
     *            have the full name.
     *            <ul>
     *            <li><code>true</code> &rarr;
     *            <code>{org, org.codecover, org.codecover.instrumentation}</code></li>
     *            <li><code>false</code> &rarr;
     *            <code>{org, codecover, instrumentation}</code></li>
     *            </ul>
     * 
     * @return A List containing the names of the packages and its super
     *         packages.
     * 
     * @see #addHierarchyLevel(HierarchyLevel, LinkedList)
     * @see #addHierarchyLevels(Collection, LinkedList)
     */
    public static LinkedList<String> packagePathToList(String fullPackagePath,
            boolean fullPackageNames) {
        if (fullPackagePath.startsWith(SEPARATOR)) {
            String message = "fullPackagePath.startsWith(Character.toString(separator))";
            throw new IllegalArgumentException(message);
        }
        if (fullPackagePath.endsWith(SEPARATOR)) {
            String message = "fullPackagePath.endsWith(Character.toString(separator))";
            throw new IllegalArgumentException(message);
        }

        LinkedList<String> packageQueue = new LinkedList<String>();
        // special case: fullPackagePath == ""
        if (fullPackagePath.length() == 0) {
            return packageQueue;
        }

        String[] packageNameArray = fullPackagePath.split(SEPARATOR_REG_EXP);
        for (int i = 0; i < packageNameArray.length; i++) {
            if (fullPackageNames && i > 0) {
                packageNameArray[i] = packageNameArray[i - 1] + SEPARATOR + packageNameArray[i]; 
            }
            packageQueue.add(packageNameArray[i]);
        }

        return packageQueue;
    }
}