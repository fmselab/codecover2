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

package org.codecover.report.highlighting;

import java.util.List;


/**
 * Represents an excerpt of a SourceFile with the file name and a List of
 * text lines found in a plain text file with some additional information 
 * (see {@link TextLine}).
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: ExtractOfCodeFile.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExtractOfCodeFile {
    
    private final String fileName;
    private final List<TextLine> textLines;
    
    public ExtractOfCodeFile(String fileName, List<TextLine> textLines) {
        this.fileName = fileName;
        this.textLines = textLines;
    }

    /**
     * @return the name of the file
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @return the lines of text in this excerpt of the file
     */
    public List<TextLine> getTextLines() {
        return textLines;
    }

}