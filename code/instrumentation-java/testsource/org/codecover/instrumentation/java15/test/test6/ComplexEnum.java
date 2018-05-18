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

package org.codecover.instrumentation.java15.test.test6;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: ComplexEnum.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
enum SimpleEnum {
    AUDI_A3, AUDI_A4, AUDI_A6, AUDI_A8,;
}

public enum ComplexEnum {
    
    AUDI_A4_AVANT, AUDI_RS4, AUDI_RS4_AVANT, AUDI_A8_W12, AUDI_RS8;
    
    public static class ClassInComplexEnum {
        public ComplexEnum getFavourite() {
            int i = 0;
            i++;
            return ComplexEnum.AUDI_RS4_AVANT;
        }
    }

    public static final int CONSTANT = Integer.valueOf(0);
    static {;}

    private transient Boolean CONSTANT_TRUE = Boolean.TRUE;
    {}

    public static final int getConstantInteger() {
        return CONSTANT;
    }

    public final boolean getConstantBoolean() {
        return CONSTANT_TRUE.booleanValue() ^ false == CONSTANT_TRUE.booleanValue();
    }

    private ComplexEnum() {
        CONSTANT_TRUE = Boolean.FALSE;
    }
    
    public enum OtherEnum {
        OTHERS;
        
        static int j;
    }
}
