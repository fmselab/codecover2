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

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.regex.Pattern;

import javax.management.MBeanServer;

import org.codecover.instrumentation.InstrumenterDirective;
import org.codecover.instrumentation.java.measurement.ProtocolImpl;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.criteria.SynchronizedStatementCoverage;
import org.codecover.model.utils.criteria.QMOCoverage;
import org.codecover.model.utils.file.FileTool;

/**
 * A {@link org.codecover.instrumentation.InstrumenterDescriptor} for
 * {@link org.codecover.instrumentation.java15.Instrumenter}.<br>
 * <br>
 * This descriptor supports Java 1.5, {@link StatementCoverage},
 * {@link BranchCoverage}, {@link ConditionCoverage} and {@link LoopCoverage}.
 * 
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: InstrumenterDescriptor.java 69 2010-01-27 19:31:18Z schmidberger $)
 */
public class InstrumenterDescriptor extends
org.codecover.instrumentation.InstrumenterDescriptor {

    private static final String INSTRUMENTER_UNIQUE_KEY = "CodeCover_Java_1.5";

    private static final String PROGRAMMING_LANGUAGE = "Java 1.5";

    private static final Pattern NAME_PATTERN = Pattern.compile("Java(( )?(1\\.)?)5?",
            Pattern.CASE_INSENSITIVE);

    private static final String AUTHOR = "Christoph Mueller";

    private static final String INSTRUMENTER_VERSION = "1.0";

    private static final String DESCRIPTION = "An instrumenter for Java 1.5.\n"
        + "Source files have to have the extension \".java\"\n"
        + "This instrumenter is part of the release of CodeCover. It supports "
        + "Statement, Branch, Condition and Loop Coverage. Long arrays are "
        + "used to keep the counters.\n"
        + "Attention: for Java 1.4 compatibility use the instrumenter directive "
        + "\"-D Java1.4=true\".\n"
        + "by: " + AUTHOR
        + "\ninstrumenter version: " + INSTRUMENTER_VERSION;

    private static final Charset DEFAULT_CHARSET = Charset.defaultCharset();

    /**
     * Creates a new {@link InstrumenterDescriptor} and sets the properties.
     */
    public InstrumenterDescriptor() {
        super(INSTRUMENTER_UNIQUE_KEY);
        super.setLanguageName(PROGRAMMING_LANGUAGE);
        super.setDescription(DESCRIPTION);
        super.setAuthor(AUTHOR);
        super.setDefaultCharset(DEFAULT_CHARSET);

        super.addSupportedCriteria(StatementCoverage.getInstance());
        super.addSupportedCriteria(BranchCoverage.getInstance());
        super.addSupportedCriteria(ConditionCoverage.getInstance());
        super.addSupportedCriteria(LoopCoverage.getInstance());
        super.addSupportedCriteria(SynchronizedStatementCoverage.getInstance());
        super.addSupportedCriteria(QMOCoverage.getInstance());

        super.registerDirective(CoverageLogPathDirective.INSTANCE);
        super.registerDirective(Java14Compatibility.INSTANCE);
    }

    @Override
    public boolean isLanguageSupported(String languageNameToCheck) {
        return NAME_PATTERN.matcher(languageNameToCheck).matches();
    }

    @Override
    protected Instrumenter getInstrumenter() {
        return new Instrumenter();
    }

    @Override
    public boolean accept(File file) {
        // the extension is in small letters and has to be java
        return FileTool.getExtension(file).equals("java");
    }

    /**
     * A directive to set the name of the coverage log file while instrumenting.
     * 
     * @author Christoph Müller 
     *
     * @version 1.0 ($Id: InstrumenterDescriptor.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    public static class CoverageLogPathDirective extends InstrumenterDirective {

        /** The name of this directive: {@value} */
        public static final String KEY = "CoverageLogPath";

        private static final String DIRECTIVE_DESCRIPTION =
            "This directive is used to set the path of the coverage log file, " +
            "the instrumented files will use for default.";

        static final CoverageLogPathDirective INSTANCE = new CoverageLogPathDirective();
        
        /**
         * Constructor for a new {@link CoverageLogPathDirective}.
         */
        public CoverageLogPathDirective() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            try {
                return new File(value).getCanonicalPath();
            } catch (IOException e) {
                throw new IllegalArgumentException(e);
            }
        }
    }

    /**
     * A directive to activate compatibility with Java 1.4.<br>
     * <br>
     * The only effect is yet, that the {@link JavaInstrumentationHelper} is
     * ordered to copy a Version of <code>ProtocolImpl.java14</code> instead of
     * {@link ProtocolImpl}, which uses an {@link MBeanServer}&mdash;a not 
     * Java 1.4 compatible class.
     * 
     * @author Christoph Müller 
     *
     * @version 1.0 ($Id: InstrumenterDescriptor.java 69 2010-01-27 19:31:18Z schmidberger $)
     */
    public static class Java14Compatibility extends InstrumenterDirective {

        /** The name of this directive: {@value} */
        public static final String KEY = "Java1.4";

        private static final String DIRECTIVE_DESCRIPTION =
            "This directive is used to indicate Java 1.4 compatibility. " +
            "Use this directive only, if you need Java 1.4 compatibility - e.g. " +
            KEY + "=true";

        static final Java14Compatibility INSTANCE = new Java14Compatibility();

        /**
         * Constructor for a new {@link Java14Compatibility}.
         */
        public Java14Compatibility() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            if (value.equalsIgnoreCase("false") ||
                value.equalsIgnoreCase("no") |
                value.equalsIgnoreCase("off")) {
                return Boolean.FALSE;
            }
            return Boolean.TRUE;
        }

        @Override
        public Object getDefaultValue() {
            return Boolean.FALSE;
        }
    }
}
