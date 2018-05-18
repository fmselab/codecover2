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

package org.codecover.instrumentation.java15.syntaxtree;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.instrumentation.java15.parser.Token;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: NodeTokenTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class NodeTokenTest extends TestCase {

    public void testCreate1() throws Exception {
        Token token = new Token();
        String sfImage = "if";
        String parsedImage = sfImage;
        token.setImages(sfImage, parsedImage);
        NodeToken nt = NodeToken.createToken(token);
        Assert.assertSame(sfImage, nt.getSourceFileImage());
        Assert.assertSame(parsedImage, nt.getParsedImage());
    }

    public void testCreate2() throws Exception {
        Token token = new Token();
        String sfImage = "\\u0070f";
        String parsedImage = "if";
        token.setImages(sfImage, parsedImage);
        NodeToken nt = NodeToken.createToken(token);
        Assert.assertSame(sfImage, nt.getSourceFileImage());
        Assert.assertSame(parsedImage, nt.getParsedImage());
    }
}
