///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumenterDescriptor.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 16:23:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.util.Collections;
import java.util.Set;
import java.util.TreeSet;

import org.gbt2.instrumentation.criteria.Criterion;

/**
 * @author Christoph Müller
 * 
 */
public class InstrumenterDescriptor {
    /** The name of the programming language, this Instrumenter supports. */
    protected String languageName;

    /** A description of this instrumenter. */
    protected String description;

    /** The author of this instrumenter. */
    protected String author;

    /** The version of this instrumeter. */
    protected String version;

    /** An set with all criteria this instrumeter supports. */
    protected Set<Criterion> supportedCriteria;

    /**
     * protected constructor for initializing all member fields.
     */
    protected InstrumenterDescriptor() {
        this.languageName = null;
        this.description = null;
        this.author = null;
        this.version = null;
        this.supportedCriteria = new TreeSet<Criterion>();
    }

    /**
     * @return The name of the programming language, this Instrumenter supports.
     */
    public String getLanguageName() {
        return this.languageName;
    }

    /**
     * @param languageName the languageName to set
     */
    public void setLanguageName(String languageName) {
        this.languageName = languageName;
    }

    /**
     * @return A description of this instrumenter.
     */
    public String getDescription() {
        return this.description;
    }

    /**
     * @param description the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return The author of this instrumenter.
     */
    public String getAuthor() {
        return this.author;
    }

    /**
     * @param author the author to set
     */
    public void setAuthor(String author) {
        this.author = author;
    }

    /**
     * @return The version of this instrumeter.
     */
    public String getVersion() {
        return this.version;
    }

    /**
     * @param version the version to set
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * @return An <b>unmodifiable</b> set with all criteria this instrumeter
     *         supports.
     */
    public Set<Criterion> getSupportedCriteria() {
        return Collections.unmodifiableSet(this.supportedCriteria);
    }

    /**
     * Asks, whether a criterion is supported.
     * 
     * @param criterion The given criterion.
     * @return true &rarr; supported; false &rarr; not supported
     */
    public boolean isCriterionSupported(Criterion criterion) {
        return this.supportedCriteria.contains(criterion);
    }

    /**
     * @param supportedCriteria the supportedCriteria to set
     */
    public void addSupportedCriteria(Criterion criterion) {
        this.supportedCriteria.add(criterion);
    }
}
