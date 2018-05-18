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

/**
 * A code file with no package declaration<br>
 * 
 * @author Igor Podolskiy 
 *
 * @version 1.0 ($Id: TestClass.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * @see <a href="http://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=136">Bug 136</a>
 */
public class TestClass {

    public static void main(String[] args) throws Exception {
        if (args.length == 0) {
            System.out.println("No Args");
        } else {
            for (String thisArg : args) {
                System.out.println(thisArg);
            }
        }

        helloWorld();
    }

    public static void helloWorld() {
        System.out.println("Hello world!"); 
    }
}
