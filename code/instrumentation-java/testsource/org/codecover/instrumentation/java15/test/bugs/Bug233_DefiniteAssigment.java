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
 * @version 1.0 ($Id: Bug233_DefiniteAssigment.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * See http://stupro.selfhost.eu/BugzillaStuPro/show_bug.cgi?id=233.
 * 
 * Instrumenter is changing definite assignment semantics of certain Java
 * programs through instrumentation.
 * 
 * The test methods are called after the number of the bug example in Bugzilla.
 * From down to top
 */
public class Bug233_DefiniteAssigment {
    final int i;

    Bug233_DefiniteAssigment(boolean b) {
        while (b || true)
            ;
    }

    void Bug233_2() {
        boolean b = false;
        int x;
        if ((b &= false) && false)
            x++;
    }

    void Bug233_3() {
        boolean b = true;
        int x;
        if ((b |= true) || true)
            ;
        else
            x++;
    }

    void Bug233_4() {
        boolean b = true;
        final int x;
        x = 0;
        if ((b |= true) || true)
            ;
        else
            x = 1;
    }

    void Bug233_5() {
        boolean b = false;
        final int x;
        x = 0;
        if ((b &= false) && false)
            x = 1;
    }

    void Bug233_6() {
        boolean x, y = true;
        if (y && false)
            y = x;
    }

    void Bug233_7() {
        boolean x, y = true;
        if (false && y)
            y = x;
    }

    void Bug233_8() {
        boolean x, y = true;
        if (true && (true ? true : y))
            ;
        else
            y = x;
    }

    void Bug233_9_1613dap1() {
        boolean x, y = false;
        if (y || true)
            ;
        else
            y = x;
    }

    void Bug233_9_1613dap2() {
        boolean x, y = false;
        if (true || y)
            ;
        else
            y = x;
    }

    void Bug233_9_1613dap3() {
        boolean x, y = false;
        if (false || (false ? y : false))
            y = x;
    }

    void Bug233_9_16210dup1() {
        final int i;
        boolean b = true;
        do {
            if (b)
                break;
            try {
                i = 1;
                break; // doesn't exit do
            } finally {
                return;
            }
        } while (true);
        i = 2;
    }

    void Bug233_9_16210dup8() {
        final int i;
        boolean b = true;
        do
            i = 1;
        while (b && false);
    }

    void Bug233_9_16210dup9() {
        final boolean b;
        b = true;
        do
            ; while (b || true);
        b = false;
    }

    void Bug233_9_16210dap1() {
        int i;
        boolean b = true;
        do
            ; while (b || true);
        int j = i;
    }

    void Bug233_9_16211dup1() {
        final int i;
        boolean b = true;
        for (;;) {
            if (b)
                break;
            try {
                i = 1;
                break; // doesn't exit for
            } finally {
                return;
            }
        }
        i = 2;
    }

    void Bug233_9_16211dup8() {
        boolean b = true;
        for (final int i; b && false;)
            i = 1;
    }

    void Bug233_9_16211dup9() {
        final boolean b;
        for (b = true; b || true;)
            ;
        b = false;
    }

    void Bug233_9_16211dap1() {
        int i;
        for (boolean b = true; b || true;)
            ;
        int j = i;
    }

    void Bug233_9_1628f1(int i) {
        switch (i) {
        case 0:
            final int j;
            j = "".length();
            break;
        case 1:
            j = 1;
        }
    }

    void Bug233_9_1629dup7() {
        final int i;
        boolean b = true;
        while (b && false)
            i = 1;
    }

    void Bug233_9_1629dup8() {
        final boolean b;
        b = true;
        while (b || true)
            ;
        b = false;
    }

    void Bug233_9_1629dap1() {
        int i;
        boolean b = true;
        while (b || true)
            ;
        int j = i;
    }
}
