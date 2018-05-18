package org.codecover.instrumentation.c;

import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterDirective;
import org.codecover.model.utils.criteria.*;
import org.codecover.model.utils.file.FileTool;

import java.io.File;
import java.nio.charset.Charset;

public class InstrumenterDescriptor extends org.codecover.instrumentation.InstrumenterDescriptor {

    private static final String INSTRUMENTER_UNIQUE_KEY = "CodeCover_C";

    private static final String PROGRAMMING_LANGUAGE = "C";

    private static final String AUTHOR = "Steffen Hanikel";

    private static final String INSTRUMENTER_VERSION = "1.0";

    private static final String DESCRIPTION = "An instrumenter for C.\n"
            + "Source files have to have the extension \".c\" or \".C\"\n"
            + "This instrumenter is part of the release of CodeCover. It supports "
            + "Statement, Branch, Condition and Loop Coverage. Int arrays are "
            + "used to keep the counters.\n"
            + "by: " + AUTHOR
            + "\ninstrumenter version: " + INSTRUMENTER_VERSION;

    private static final Charset DEFAULT_CHARSET = Charset.defaultCharset();

    /**
     * protected constructor for initializing all member fields.
     */
    protected InstrumenterDescriptor() {
        super(INSTRUMENTER_UNIQUE_KEY);
        setLanguageName(PROGRAMMING_LANGUAGE);
        setDescription(DESCRIPTION);
        setAuthor(AUTHOR);
        setDefaultCharset(DEFAULT_CHARSET);

        addSupportedCriteria(StatementCoverage.getInstance());
        addSupportedCriteria(BranchCoverage.getInstance());
        addSupportedCriteria(LoopCoverage.getInstance());
        addSupportedCriteria(ConditionCoverage.getInstance());
        addSupportedCriteria(QMOCoverage.getInstance());

        registerDirective(IncludeDirs.INSTANCE);
        registerDirective(Defines.INSTANCE);
        registerDirective(Types.INSTANCE);
        registerDirective(Debug.INSTANCE);
    }

    @Override
    public boolean isLanguageSupported(String languageNameToCheck) {
        return "C".equalsIgnoreCase(languageNameToCheck);
    }

    @Override
    protected Instrumenter getInstrumenter() {
        return new org.codecover.instrumentation.c.Instrumenter();
    }

    @Override
    public boolean accept(File file) {
        return FileTool.getExtension(file).equalsIgnoreCase("c");
    }

    public static class IncludeDirs extends InstrumenterDirective {
        public static final String KEY = "IncludeDirs";

        private static final String DIRECTIVE_DESCRIPTION =
                "Use this directive to add include paths. " +
                        "Specify them separated by a semicolon - e.g. " +
                        KEY + "=DIR;DIR";

        static final IncludeDirs INSTANCE = new IncludeDirs();

        public IncludeDirs() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            return value.split(";");
        }

        @Override
        public Object getDefaultValue() {
            return new String[0];
        }
    }

    public static class Defines extends InstrumenterDirective {
        public static final String KEY = "Defines";

        private static final String DIRECTIVE_DESCRIPTION =
                "Use this directive to add additonal defines. " +
                        "Specify them separated by a colon - e.g. " +
                        KEY + "=DEFINE:DEFINE";

        static final Defines INSTANCE = new Defines();

        public Defines() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            return value.split(":");
        }

        @Override
        public Object getDefaultValue() {
            return new String[0];
        }
    }

    public static class Types extends InstrumenterDirective {
        public static final String KEY = "Types";

        private static final String DIRECTIVE_DESCRIPTION =
                "Add predefined types. " +
                        "Specify them separated by a colon - e.g. " +
                        KEY + "=ID:ID";

        static final Types INSTANCE = new Types();

        public Types() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            return value.split(":");
        }

        @Override
        public Object getDefaultValue() {
            return new String[0];
        }
    }

    public static class Debug extends InstrumenterDirective {
        public static final String KEY = "Debug";

        private static final String DIRECTIVE_DESCRIPTION =
                "Set to true to use the debug parser.";

        static final Debug INSTANCE = new Debug();

        public Debug() {
            super(KEY, DIRECTIVE_DESCRIPTION);
        }

        @Override
        public Object parseValue(String value) throws IllegalArgumentException {
            return value.equalsIgnoreCase("true") || value.equalsIgnoreCase("1");
        }

        @Override
        public Object getDefaultValue() {
            return false;
        }
    }
}
