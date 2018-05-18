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

package org.codecover.instrumentation.java15.test.test5;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: ComplexInterface.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
interface SimpleInterface {
    public Boolean toBool();

    public String toString();

    ;
}

public interface ComplexInterface extends SimpleInterface {
    public static int CONSTANT = 0;

    public int member = 0;

    public Long toLong();

    public class ComplexUser implements ComplexInterface {
        public Boolean toBool() {
            System.out.println(ComplexUser.class.getName());
            return Boolean.TRUE;
        }

        public String toString() {
            System.out.println(ComplexUser.class.getName());
            return new String("String");
        }

        public Long toLong() {
            System.out.println(ComplexUser.class.getName());
            return Long.valueOf(this.member);
        }

        static int juhu = 0;

        static {
            ;
        }
    }
}
 