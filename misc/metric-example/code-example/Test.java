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

public class Test {
    public static void main(String[] args) {
        System.out.println("Hello, world!");
        
        partlyExecutedMethod();
    }
    
    static void partlyExecutedMethod() {
        System.out.println("Test");

        System.out.println("Test");
        
        if (0 == 0)
            return;
        
        System.out.println("Test");

        System.out.println("Test");

        System.out.println("Test");
    }

    
    static void notExecutedMethod() {
        System.out.println("Test");

        System.out.println("Test");

        System.out.println("Test");
        
        if (0 == 0)
            return;
        
        System.out.println("Test");

        System.out.println("Test");

        System.out.println("Test");
    }
}
