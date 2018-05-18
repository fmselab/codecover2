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
 * MetaDataObject is implemented by all classes to which meta data can be
 * associated. It has only one method (getMetaData()) which can be used to get
 * the MetaData. Instances of this class can be passed to the methods
 * setObjectMetaData(...) and getObjectMetaData(...) of the class TestCase.
 * 
 * @author Markus Wittlinger
 * @version 1.0 ($Id: MetaDataObject.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public interface MetaDataObject {
    /**
     * Gets the {@link MetaData} used in storing data.
     * 
     * @return the {@link MetaData}
     */
    public MetaData getMetaData();
}
