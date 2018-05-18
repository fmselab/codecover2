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

package org.codecover.instrumentation.java15;

import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;

/**
 * @author Christoph Müller
 * @version 1.0 ($Id: HierarchyLevelTypeProvider.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HierarchyLevelTypeProvider {

    private static final String DEFAULT_PACKAGE_INTERN = "default package";

    private static final String DEFAULT_PACKAGE_ENGLISH = "default package";

    private static final String PACKAGE_INTERN = "package";

    private static final String PACKAGE_ENGLISH = "package";

    private static final String CLASS_INTERN = "class";

    private static final String CLASS_ENGLISH = "class";

    private static final String ANONYMOUS_CLASS_INTERN = "class";

    private static final String ANONYMOUS_CLASS_ENGLISH = "anonymous class";
    
    private static final String INTERFACE_INTERN = "interface";

    private static final String INTERFACE_ENGLISH = "interface";

    private static final String ENUM_INTERN = "enum";

    private static final String ENUM_ENGLISH = "enumeration";

    private static final String ENUM_CONSTANT_INTERN = "enum constant";

    private static final String ENUM_CONSTANT_ENGLISH = "enumeration constant";

    private static final String ANNOTATION_INTERN = "@interface";

    private static final String ANNOTATION_ENGLISH = "annotation";

    private static final String METHOD_INTERN = "method";

    private static final String METHOD_ENGLISH = "method";

    private HierarchyLevelType defaultPackageType = null;

    private HierarchyLevelType packageType = null;

    private HierarchyLevelType classType = null;

    private HierarchyLevelType anonymousClassType = null;

    private HierarchyLevelType interfaceType = null;

    private HierarchyLevelType enumType;

    private HierarchyLevelType enumConstantType;

    private HierarchyLevelType annotationType;

    private HierarchyLevelType methodType = null;

    private MASTBuilder builder;

    /**
     * Creates a new instance of {@link HierarchyLevelTypeProvider} using a
     * given {@link MASTBuilder}.
     * 
     * @param database
     *            The {@link MASTBuilder} to use.
     */
    public HierarchyLevelTypeProvider(MASTBuilder database) {
        this.builder = database;
    }

    /**
     * Returns the hierarchy level type for a default package.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getDefaultPackageType() {
        if (this.defaultPackageType == null) {
            this.defaultPackageType = this.builder.createHierarchyLevelType(
                    DEFAULT_PACKAGE_ENGLISH, DEFAULT_PACKAGE_INTERN);
        }
        return this.defaultPackageType;
    }

    /**
     * Returns the hierarchy level type for a package.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getPackageType() {
        if (this.packageType == null) {
            this.packageType = this.builder.createHierarchyLevelType(
                    PACKAGE_ENGLISH, PACKAGE_INTERN);
        }
        return this.packageType;
    }

    /**
     * Returns the hierarchy level type for a class.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getClassType() {
        if (this.classType == null) {
            this.classType = this.builder.createHierarchyLevelType(
                    CLASS_ENGLISH, CLASS_INTERN);
        }
        return this.classType;
    }

    /**
     * Returns the hierarchy level type for an anonymous class.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getAnonymousClassType() {
        if (this.anonymousClassType == null) {
            this.anonymousClassType = this.builder.createHierarchyLevelType(
                    ANONYMOUS_CLASS_ENGLISH, ANONYMOUS_CLASS_INTERN);
        }
        return this.anonymousClassType;
    }

    /**
     * Returns the hierarchy level type for an interface.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getInterfaceType() {
        if (this.interfaceType == null) {
            this.interfaceType = this.builder.createHierarchyLevelType(
                    INTERFACE_ENGLISH, INTERFACE_INTERN);
        }
        return this.interfaceType;
    }

    /**
     * Returns the hierarchy level type for an enum.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getEnumType() {
        if (this.enumType == null) {
            this.enumType = this.builder.createHierarchyLevelType(
                    ENUM_ENGLISH, ENUM_INTERN);
        }
        return this.enumType;
    }

    /**
     * Returns the hierarchy level type for an enum constant type.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getEnumConstantType() {
        if (this.enumConstantType == null) {
            this.enumConstantType = this.builder.createHierarchyLevelType(
                    ENUM_CONSTANT_ENGLISH, ENUM_CONSTANT_INTERN);
        }
        return this.enumConstantType;
    }

    /**
     * Returns the hierarchy level type for an annotation.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getAnnotationType() {
        if (this.annotationType == null) {
            this.annotationType = this.builder.createHierarchyLevelType(
                    ANNOTATION_ENGLISH, ANNOTATION_INTERN);
        }
        return this.annotationType;
    }

    /**
     * Returns the hierarchy level type for a method.
     * 
     * @return the hierarchy level type
     */
    public HierarchyLevelType getMethodType() {
        if (this.methodType == null) {
            this.methodType = this.builder.createHierarchyLevelType(
                    METHOD_ENGLISH, METHOD_INTERN);
        }
        return this.methodType;
    }
}
