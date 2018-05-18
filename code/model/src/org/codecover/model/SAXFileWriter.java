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

package org.codecover.model;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXSource;
import javax.xml.transform.stream.StreamResult;

import org.xml.sax.XMLReader;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: SAXFileWriter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class SAXFileWriter {

    /**
     * Writes the given {@link TestSessionContainer} to the location specified
     * by the given {@link File}.
     * 
     * @param testSessionContainer
     *            the {@link TestSessionContainer} to write.
     * @param file
     *            the location, the {@link TestSessionContainer} is written to.
     * @param xmlReader
     *            the {@link XMLReader} to be used in creating the sax events.
     * @throws IOException
     * @throws TransformerException
     */
    public static void write(TestSessionContainer testSessionContainer,
            File file, XMLReader xmlReader) throws IOException,
            TransformerException {
        FileOutputStream out = new FileOutputStream(file);

        try {
            // Use a Transformer for output
            TransformerFactory tf = TransformerFactory.newInstance();
            Transformer serializer = tf.newTransformer();

            // Use the parser as a SAX source for input
            TestSessionContainerInputSource inputSource = new TestSessionContainerInputSource(
                    testSessionContainer);

            SAXSource source = new SAXSource(xmlReader, inputSource);
            StreamResult result = new StreamResult(out);
            serializer.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            serializer.setOutputProperty(OutputKeys.INDENT, "yes");
            serializer.transform(source, result);
            out.flush();
        } finally {
            out.close();
        }
    }
}