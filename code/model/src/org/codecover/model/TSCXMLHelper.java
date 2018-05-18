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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.List;
import java.util.Vector;

import org.codecover.model.mast.BooleanAssignment;
import org.codecover.model.mast.BooleanResult;
import org.codecover.model.utils.Base64;
import org.codecover.model.utils.Logger;

/**
 * @author Markus Wittlinger
 * @version 1.0 ($Id: TSCXMLHelper.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class TSCXMLHelper {

    /**
     * Decodes the {@link BooleanAssignment} of the given encoded string
     * 
     * @param encodedBooleanAssignment
     *            the given string holding the {@link BooleanAssignment}
     * @return the decoded {@link BooleanAssignment}
     */
    protected static final BooleanAssignment decodeBooleanAssignment(
            String encodedBooleanAssignment, Logger logger) {
        List<BooleanResult> results = new Vector<BooleanResult>();

        for (Character character : encodedBooleanAssignment.toCharArray()) {
            Integer integer = Integer.parseInt(character.toString());
            for (BooleanResult booleanResult : BooleanResult.values()) {
                if (integer.intValue() == booleanResult.ordinal()) {
                    results.add(booleanResult);
                    break;
                }
            }
        }

        return new BooleanAssignment(results);
    }

    /**
     * Decodes the given Base64 string.
     * 
     * @param base64
     *            the string to decode.
     * @return the decoded object.
     */
    protected static final Object decodeMetaData(String base64, Logger logger) {
        final Object serialisedObject;

        final ByteArrayInputStream dataStream = new ByteArrayInputStream(Base64
                .decode(base64));

        try {
            final ObjectInputStream objectStream = new ObjectInputStream(
                    dataStream);

            serialisedObject = objectStream.readObject();
        } catch (IOException e) {
            throw new RuntimeException("Could not deserialise object", e);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException("Could not deserialise object", e);
        }

        return serialisedObject;
    }

    /**
     * Encodes the given {@link BooleanAssignment} into a String. Every entry in
     * the list of {@link BooleanResult}s is represented by it ordinal value
     * 
     * @param booleanAssignment
     *            the to be encoded {@link BooleanAssignment}
     * @return the encoded {@link BooleanAssignment}
     */
    protected static final String encodeBooleanAssignment(
            BooleanAssignment booleanAssignment, Logger logger) {
        String encodedBooleanAssignment = "";

        for (BooleanResult booleanResult : booleanAssignment.getResults()) {
            encodedBooleanAssignment += booleanResult.ordinal();
        }

        return encodedBooleanAssignment;
    }

    /**
     * Encodes the given {@link Object} in a Base64 string.
     * 
     * @param value
     *            the object to encode. <br>
     *            NOTE: The object must implement java.io.Serializable
     * @return the encoded String.
     */
    protected static final String encodeMetaData(Object value, Logger logger) {
        if (value == null) {
            throw new NullPointerException("value == null");
        }

        final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        final ObjectOutputStream objectStream;

        try {
            objectStream = new ObjectOutputStream(outputStream);

            objectStream.writeObject(value);
            objectStream.flush();
            objectStream.close();
        } catch (IOException e) {
            logger.error("Could not serialize object", e);
            return null;
        }
        return Base64.encodeBytes(outputStream.toByteArray());
    }
}
