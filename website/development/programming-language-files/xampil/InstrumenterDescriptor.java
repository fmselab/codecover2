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

package org.codecover.instrumentation.xampil;

/**
 * The instrumenter discriptor contains information about the corresponding instrumenter,
 * for example the author, the name of the programming language, the charset, the
 * instrumenter directives or the supported code coverage criteria. CodeCover asks
 * the discriptor if the instrumenter matches the programming language the user wants
 * to instrument.
 *
 * @author Stefan Franke
 */
public class InstrumenterDescriptor extends org.codecover.instrumentation.InstrumenterDescriptor {
    private static final String INSTRUMENTER_UNIQUE_KEY = "CodeCover_Xampil_2007";
    private static final String LANGUAGE = "Xampil 2007";
    private static final Pattern NAME_PATTERN = Pattern.compile("Xampil(( )?(20)?07)?",
            Pattern.CASE_INSENSITIVE);
    private static final String DESCRIPTION = "Instrumenter for Xampil.";
    private static final String AUTHOR = "Author of this guide";
    private static final Charset DEFAULT_CHARSET = Charset.defaultCharset();

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
    }

    @Override
    public boolean isLanguageSupported(String languageNameToCheck) {
        return NAME_PATTERN.matcher(languageNameToCheck).matches();
    }

    @Override
    protected Instrumenter getInstrumenter() {
        return new org.codecover.instrumentation.xampil.Instrumenter();
    }

    @Override
    public boolean accept(File file) {
        return FileTool.getExtension(file).equals("xpl");
    }
}