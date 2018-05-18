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

package org.codecover.instrumentation.java15.test.bugs;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: LoopInThen.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * Do we have problem, when we have an while in an if?<br>
 * Exprected Output: "RETURN"
 */
public class LoopInThen {

    public static void main(String[] args) {
        final boolean condition = (1 == 2);
        boolean condition2 = true;
        if (condition)
            while (condition2) {
                System.out.print("IN WHILE ");
                condition2 = !condition2;
            }
        System.out.print("RETURN");

        return;
    }
}
