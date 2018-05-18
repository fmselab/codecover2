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

import java.io.File;
import java.io.FileFilter;
import java.nio.charset.Charset;
import java.nio.charset.IllegalCharsetNameException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Map.Entry;

import org.codecover.model.utils.criteria.Criterion;

/**
 * This is a descriptor for a specific {@link Instrumenter}.<br>
 * <br>
 * It's purpose is to provide information of the corresponding Instrumenter and
 * get to know, if it fits to the programming language and {@link Criterion},
 * that should be used.<br>
 * Child classes use the various set methods to set the specific information.<br>
 * Every {@link InstrumenterDescriptor} has a unique key to identify it.<br>
 * The method {@link #isLanguageSupported(String)} checks whether the name
 * of a given language fits to this {@link InstrumenterDescriptor}. Child
 * classes can overwrite this method&mdash;e.g. to implement pattern matches.<br>
 * The {@link #getInstrumenter()} is abstract and should return a new
 * instrumenter object, that fits to the information provided by the child
 * descriptor.<br>
 * The {@link #accept(File)} method required by {@link FileFilter#accept(File)} 
 * is needed to decide, whether a file might be a source file in the given
 * programming language an by the way might be instrumentable.<br>
 * The interface {@link Comparable} is needed to put InstrumenterDescriptors
 * into a {@link TreeSet}.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrumenterDescriptor.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class InstrumenterDescriptor implements
        Comparable<InstrumenterDescriptor>, FileFilter {

    /**
     * A non null unique key to identify different
     * {@link InstrumenterDescriptor}s.
     */
    private String uniqueKey;

    /** The name of the programming language, this Instrumenter supports. */
    private String languageName;

    /** A description of this instrumenter. */
    private String description;

    /** The author of this instrumenter. */
    private String author;

    /** The {@link Charset}, which is used for the source files per default. */
    private Charset defaultCharset;

    /** A set with all criteria this instrumenter supports. */
    private Set<Criterion> supportedCriteria;

    /** Instrumenter directives and their description registered. */
    private Map<String, InstrumenterDirective> instrumenterDirectives;

    /**
     * protected constructor for initializing all member fields.
     * 
     * @param uniqueKey A key to make this {@link InstrumenterDescriptor} unique
     *                  but identifiable. 
     */
    protected InstrumenterDescriptor(String uniqueKey) {
        if (uniqueKey == null) {
            throw new NullPointerException("uniqueKey is null");
        }
        this.uniqueKey = uniqueKey;
        this.languageName = null;
        this.description = null;
        this.author = null;
        this.supportedCriteria = new TreeSet<Criterion>();
        this.instrumenterDirectives = new TreeMap<String, InstrumenterDirective>();

        registerDirective(UUIDDirective.INSTANCE);
    }

    /**
     * Returns the unique key of this {@link InstrumenterDescriptor}.
     * 
     * @return The unique key.
     */
    public String getUniqueKey() {
        return this.uniqueKey;
    }

    /**
     * @return The name of the programming language, this Instrumenter supports.
     */
    public String getLanguageName() {
        return this.languageName;
    }

    /**
     * Does this Instrumenter support a programming language given by a name?<br>
     * <br>
     * Can be overwritten by child classes.
     * 
     * @param languageNameToCheck The name of the programming language.
     * 
     * @return true &rarr; yes; false &rarr; no 
     */
    public boolean isLanguageSupported(String languageNameToCheck) {
        return this.languageName != null && this.languageName.equals(languageNameToCheck);
    }

    /**
     * @param languageName
     *            the languageName to set
     */
    protected void setLanguageName(String languageName) {
        if (languageName == null) {
            throw new NullPointerException();
        }
        this.languageName = languageName;
    }

    /**
     * @return A description of this instrumenter.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @param description
     *            the description to set
     */
    protected void setDescription(String description) {
        if (description == null) {
            throw new NullPointerException();
        }
        this.description = description;
    }

    /**
     * @return The author of this instrumenter.
     */
    public String getAuthor() {
        return this.author;
    }

    /**
     * @param author
     *            the author to set
     */
    protected void setAuthor(String author) {
        if (author == null) {
            throw new NullPointerException();
        }
        this.author = author;
    }

    /**
     * @return The charset set as default for source files.<br>
     *         If no charset is set - this charset is null.
     */
    public Charset getDefaultCharset() {
        return this.defaultCharset;
    }

    /**
     * @param charset
     *            The default charset for source files.
     */
    protected void setDefaultCharset(Charset charset) {
        if (charset == null) {
            throw new NullPointerException();
        }
        this.defaultCharset = charset;
    }

    /**
     * @param charsetName
     *            The name of the default charset for source files.
     *
     * @throws IllegalCharsetNameException
     *             If no support for the named charset is available in this
     *             instance of the Java virtual machine.
     * @see Charset#forName(String)
     */
    protected void setDefaultCharset(String charsetName)
    throws IllegalCharsetNameException {
        if (charsetName == null) {
            throw new NullPointerException();
        }
        this.defaultCharset = Charset.forName(charsetName);
    }

    /**
     * @return An <b>unmodifiable</b> set with all criteria this instrumenter
     *         supports.
     */
    public Set<Criterion> getSupportedCriteria() {
        return Collections.unmodifiableSet(this.supportedCriteria);
    }

    /**
     * Asks, whether a criterion is supported.
     * 
     * @param criterion
     *            The given criterion.
     * @return true &rarr; supported; false &rarr; not supported
     */
    public boolean isCriterionSupported(Criterion criterion) {
        return this.supportedCriteria.contains(criterion);
    }

    /**
     * @param criterion
     *            the supportedCriteria to set
     */
    protected void addSupportedCriteria(Criterion criterion) {
        if (criterion == null) {
            throw new NullPointerException();
        }
        this.supportedCriteria.add(criterion);
    }

    /**
     * Returns an unmodifiable Map with {@link InstrumenterDirective},
     * registered for this descriptor.
     * 
     * @return A unmodifiable Map with <code>key</code> &rarr;
     *         {@link InstrumenterDirective}.
     */
    public Map<String, InstrumenterDirective> getRegisteredDirectives() {
        return Collections.unmodifiableMap(this.instrumenterDirectives);
    }

    /**
     * Adds an instrumenter directive.
     *  
     * @param directive A new {@link InstrumenterDirective} to be registered.
     */
    protected void registerDirective(InstrumenterDirective directive) {
        this.instrumenterDirectives.put(directive.getKey(), directive);
    }

    /**
     * Creates a Map with default values for all registered
     * {@link InstrumenterDirective}s.<br>
     * <br>
     * For all {@link #instrumenterDirectives}, the {@link InstrumenterDirective#getDefaultValue()}
     * is called. If the value is not <code>null</code>, the value is stored
     * in a Map.
     * 
     * @return A Map with default values for all {@link InstrumenterDirective}s.
     */
    public Map<String, Object> getDefaultDirectiveValues() {
        Map<String, Object> directiveValues = new HashMap<String, Object>();
        for (Entry<String, InstrumenterDirective> directiveEntry : this.instrumenterDirectives.entrySet()) {
            Object defaultValue = directiveEntry.getValue().getDefaultValue();
            if (defaultValue != null) {
                directiveValues.put(directiveEntry.getKey(), defaultValue);
            }
        }
        return directiveValues;
    }

    /**
     * Get an instance of a {@link Instrumenter} child class, that fits to the
     * information provided by this descriptor.
     * 
     * @return A new {@link Instrumenter}.
     */
    protected abstract Instrumenter getInstrumenter();

    /**
     * Decides, whether a file should be instrumented or not.
     * 
     * @param file
     *            The file, to be checked.
     * 
     * @return true &rarr; the file should be instrumented; false &rarr; the
     *         file is no source file.
     */
    public abstract boolean accept(File file);

    public final int compareTo(InstrumenterDescriptor other) {
        // this should be unique
        return this.getUniqueKey().compareTo(other.getUniqueKey());
    }
}
