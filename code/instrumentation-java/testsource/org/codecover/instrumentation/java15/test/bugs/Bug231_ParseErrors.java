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
 * @version 1.0 ($Id: Bug231_ParseErrors.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * See http://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=231.
 * 
 * Parse Error in Java Instrumenter
 */
public class Bug231_ParseErrors {
    // Bug 231_1
    static class Outer {
        class Inner {
        }
    }

    static class ChildOfInner extends Outer.Inner {
        ChildOfInner() {
            (new Outer()).super();
        }
    }

    // Bug 231_2
    class T8851q1 {
        class Inner {
        }
    }

    class Sub1 extends T8851q1.Inner {
        Sub1() {
            new T8851q1().super();
        }
    }

    // Bug 231_3
    class T8851q2 {
        class Inner {
        }
    }

    class Sub2 extends T8851q2.Inner {
        Sub2() {
            new T8851q2() {
            }.super();
        }
    }

    // Bug 231_4
    class T8851q3 {
        class Inner {
        }
    }

    class Sub3 extends T8851q3.Inner {
        Sub3() {
            // this will not execute, but must compile
            ((T8851q3) null).super();
        }
    }

    // Bug 231_5
    class T8851q10 {

        class One {
        }

        class Two extends One {
            Two() {
                new T8851q10().super();
            }

            Two(int i) {
            }
        }

    }

    // Bug 231_6
    class Super {
        Middle m;
    }

    class Middle extends Super {
        class A {
        }

        class B extends A {
            B() {
                Middle.super.m.super();
            }
        }
    }
}
