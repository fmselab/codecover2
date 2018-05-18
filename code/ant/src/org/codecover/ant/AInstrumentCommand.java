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
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.ProgressHandler;
import org.codecover.model.utils.file.SourceTargetContainer;
import org.codecover.utils.InstrumentUtils;

/**
 * Abstract Command for the instrument Commands.
 *
 * @author Christoph Müller
 * @version 1.0 ($Id: InstrumentCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 *
 */
public abstract class AInstrumentCommand extends Command implements
        InstrumentUtils.GetFilesCallback {
    String containerId;

    String language;

    String instrumenter;

    String charset;

    boolean copyUninstrumented = false;

    boolean override = true;

    CriteriaList criteria = null;

    DirectiveList directives = null;

    /**
     * Sets the containerId.
     *
     * @param containerId
     *                the containerId to set
     */
    public void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    /**
     * Sets the language.
     *
     * @param language
     *                the language to set
     */
    public void setLanguage(String language) {
        this.language = language;
    }

    /**
     * Sets the instrumenter.
     *
     * @param instrumenter
     *                the instrumenter to set
     */
    public void setInstrumenter(String instrumenter) {
        this.instrumenter = instrumenter;
    }

    /**
     * Sets the charset.
     *
     * @param charset
     *                the charset to set
     */
    public void setCharset(String charset) {
        this.charset = charset;
    }

    /**
     * Sets the copyUninstrumented.
     *
     * @param copyUninstrumented
     *                the copyUninstrumented to set
     */
    public void setCopyUninstrumented(boolean copyUninstrumented) {
        this.copyUninstrumented = copyUninstrumented;
    }

    /**
     * Sets the override.
     *
     * @param override
     *                the override to set
     */
    public void setOverride(boolean override) {
        this.override = override;
    }

    /**
     * Adds a configured {@link CriteriaList} to this command.
     *
     * @param criteriaList
     *                the {@link CriteriaList} to add.
     */
    public void addConfiguredCriteria(CriteriaList criteriaList) {
        if (this.criteria != null) {
            throw new BuildException("There are multiple <criteria> elements");
        }
        this.criteria = criteriaList;
    }

    /**
     * Adds a configured {@link DirectiveList} to this command.
     *
     * @param directiveListList
     *                the {@link DirectiveList} to add.
     */
    public void addConfiguredDirectives(DirectiveList directiveListList) {
        if (this.directives != null) {
            throw new BuildException("There are multiple <directives> elements");
        }
        this.directives = directiveListList;
    }

    @Override
    public void run(final Context context) {
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        if (this.language == null) {
            throw new BuildException("The attribute 'language' is missing.");
        }

        /* TODO: use override */

        final Logger logger = context.getLogger();

        final ProgressHandler progressHandler = ProgressHandler.NULL;

        final String pLanguage = this.language;

        // optional options
        final boolean pHasCriteria = this.criteria != null;
        final List<String> pCriteria;
        if (pHasCriteria) {
            pCriteria = new ArrayList<String>(this.criteria.getCriteria());
        } else {
            pCriteria = Collections.<String> emptyList();
        }
        final boolean pHasCharset = this.charset != null;
        final String pCharset = this.charset;
        final boolean pCopyUninstrumented = this.copyUninstrumented;

        final List<String> pDirectives = new ArrayList<String>();
        if (this.directives != null) {
            for (Directive thisDirective : this.directives.getDirectives()) {
                pDirectives.add(thisDirective.getKey() + "=" + thisDirective.getValue());
            }
        }

        validateBeforeRun();

        final InstrumentUtils.SaveSessionContainerCallback saveSessionContainerCallback = new InstrumentUtils.SaveSessionContainerCallback() {
            public void saveSessionContainer(
                    TestSessionContainer testSessionContainer) {
                context.setTestSessionContainer(
                        AInstrumentCommand.this.containerId,
                        testSessionContainer);
            }
        };

        InstrumentUtils.instrument(logger, context.getPluginManager(),
                progressHandler, /* pretend */false, getSourceDirectory().getPath(),
                getTargetDirectory().getPath(), saveSessionContainerCallback,
                pLanguage, pHasCriteria, pCriteria, pHasCharset, pCharset,
                pCopyUninstrumented, pDirectives, this.instrumenter != null,
                this.instrumenter, this);
    }

    abstract File getSourceDirectory();

    abstract File getTargetDirectory();

    abstract void validateBeforeRun();

    static Set<SourceTargetContainer> createSourceTargetContainerSet(
            File sourceBasePath, File targetBasePath, String[] paths) {
        final Set<SourceTargetContainer> result = new HashSet<SourceTargetContainer>();
        for (String path : paths) {
            result.add(new SourceTargetContainer(new File(sourceBasePath.getPath()
                    + File.separator + path), new File(targetBasePath.getPath()
                    + File.separator + path)));
        }
        return result;
    }
}
