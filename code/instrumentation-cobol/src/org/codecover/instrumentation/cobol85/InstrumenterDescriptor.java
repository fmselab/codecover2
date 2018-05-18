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

package org.codecover.instrumentation.cobol85;

import java.io.File;
import java.nio.charset.Charset;
import java.util.regex.Pattern;

import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterDirective;
import org.codecover.instrumentation.cobol85.compilerDirectives.BWCompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.CompilerDirectivesManipulator;
import org.codecover.instrumentation.cobol85.compilerDirectives.DefaultCompilerDirectivesManipulator;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.ConditionCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;

/**
 * The instrumenter descriptor contains information about the corresponding instrumenter,
 * for example the author, the name of the programming language, the charset, the
 * instrumenter directives or the supported code coverage criteria. CodeCover asks
 * the descriptor if the instrumenter matches the programming language the user wants
 * to instrument.
 *
 * @author Stefan Franke
 * @version 1.0 ($Id: InstrumenterDescriptor.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterDescriptor extends org.codecover.instrumentation.InstrumenterDescriptor {

    private static final String INSTRUMENTER_UNIQUE_KEY = "CodeCover_COBOL_85";

    private static final String LANGUAGE = "VS COBOL 1985";

    private static final Pattern NAME_PATTERN = Pattern.compile("(VS )?COBOL(( )?(19)?85)?",
            Pattern.CASE_INSENSITIVE);

    private static final String DESCRIPTION = "Instrumenter for VS COBOL 1985.";


    private static final String AUTHOR = "Stefan Franke";

    /** The description of this instrumenter */
    private static final Charset DEFAULT_CHARSET = Charset.defaultCharset();

    /**
     * {@link InstrumenterDirective} key for parsing compiler directives.
     */
    public static final String COMPILER_DIRECTIVES_KEY = "compiler-directives";

    private static final String COMPILER_DIRECTIVES_DESCRIPTION =
        "This is a directive for compiler directives. A compiler directive " +
        "manipulator is used to save and replace compiler directives before " +
        "the instrumentation starts.";

    private static final String BW_COMPILER_DIRECTIVES_KEY = "bw";

    /**
     * Constructs a new {@link InstrumenterDescriptor}.
     */
    public InstrumenterDescriptor() {
        super(INSTRUMENTER_UNIQUE_KEY);
        super.setLanguageName(LANGUAGE);
        super.setDescription(DESCRIPTION);
        super.setAuthor(AUTHOR);

        super.setDefaultCharset(DEFAULT_CHARSET);

        super.addSupportedCriteria(StatementCoverage.getInstance());
        super.addSupportedCriteria(BranchCoverage.getInstance());
        super.addSupportedCriteria(ConditionCoverage.getInstance());
        super.addSupportedCriteria(LoopCoverage.getInstance());

        InstrumenterDirective instrumenterDirective = new InstrumenterDirective(
                COMPILER_DIRECTIVES_KEY, COMPILER_DIRECTIVES_DESCRIPTION) {

            @Override
            public CompilerDirectivesManipulator parseValue(String value)
                    throws IllegalArgumentException {
                if (value.equals(BW_COMPILER_DIRECTIVES_KEY)) {
                    return new BWCompilerDirectivesManipulator();
                }
                throw new IllegalArgumentException("Directive "
                        + COMPILER_DIRECTIVES_KEY + " " + value
                        + " is unknown.");
            }

            @Override
            public CompilerDirectivesManipulator getDefaultValue() {
                return new DefaultCompilerDirectivesManipulator();
            }
        };
        super.registerDirective(instrumenterDirective);
    }

    @Override
    public boolean isLanguageSupported(String languageNameToCheck) {
        return NAME_PATTERN.matcher(languageNameToCheck).matches();
    }

    @Override
    protected Instrumenter getInstrumenter() {
        return new org.codecover.instrumentation.cobol85.Instrumenter();
    }

    @Override
    public boolean accept(File file) {
        // we cannot decide COBOL files by extension -> allow all
        return true;
    }
}
