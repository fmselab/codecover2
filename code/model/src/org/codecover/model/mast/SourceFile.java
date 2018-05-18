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

package org.codecover.model.mast;

/**
 * A SourceFile represents a source file. It contains the fileName and the
 * content of the file.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: SourceFile.java 90 2013-02-22 13:16:45Z hanikesn $)
 */
public final class SourceFile {
    private final String fileName;

    private final String content;

    SourceFile(String fileName, String content) {
        if (fileName == null) {
            throw new NullPointerException("fileName == null");
        }
        
        if (content == null) {
            throw new NullPointerException("content == null");
        }

        this.fileName = fileName;
        this.content = content;
    }

    /**
     * @return the content
     */
    public String getContent() {
        return this.content;
    }

    /**
     * @return the fileName, without the path
     */
    public String getFileName() {
        return this.fileName;
    }

    /**
     * Returns a hash code value for the object. This method is supported for
     * the benefit of hashtables such as those provided by
     * <code>java.util.Hashtable</code>.
     * 
     * @return a hash code value for this object.
     */
    @Override
    public int hashCode() {
        return getFileName().hashCode();
    }

    @Override
    public String toString()
    {
      return String.format("%s %dB", getFileName(), getContent().length());
    }
}
