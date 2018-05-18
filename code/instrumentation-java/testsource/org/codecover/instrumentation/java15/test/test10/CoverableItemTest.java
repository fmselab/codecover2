/******************************************************************************
 * Copyright (c) 2008 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.instrumentation.java15.test.test10;

import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.METHOD;

import java.lang.annotation.Target;


/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id$)
 */
public class CoverableItemTest {

    public static void main(String[] args) {
        // startTestCase("run 1");
        int i;
        BooleanAnnotation1 annotation;

        // startTestCase("run 2");
        int j = 4;

        // startTestCase("run 3");
        Label1: Label2: {
            break Label1;
        }

        // startTestCase("run 4");
        Label3: ;

        // startTestCase("run 5");
        Label4: CoverableItemTest.class.getName();

        // startTestCase("run 6");
        Label5: i = j * j * j * j * j;

        // startTestCase("run 7");
        Label6: switch (j) {
        case 4:
        case 5:
            break Label6;
        default:
            throw new IllegalStateException();
        }

        // startTestCase("run 8");
        Label7 : if (i == j) {
            throw new IllegalStateException();
        } else {
            i = 5;
        }

        Label8 : if (i < j)
            Label9: throw new IllegalStateException();
        else
            Label10: i = 3;

        Label11 : if (i > j) throw new IllegalStateException();

        // startTestCase("run 9");
        Label12: while (i < j) break Label12;

        Label13: while (i < j) {
            i = 100;
        }

        // startTestCase("run 10");
        Label14: do {break Label14;} while (i > j);

        Label15: do i -= 48; while (i > j);

        // startTestCase("run 11");
        Label16: for (;i == j;) {
            i = 10;
            j = 5;
            break Label16;
        }

        Label17: for (;i != j;) j++;

        Label18: for (String s : new String[]{"1", "2"}) i++;

        Label19: for (String s : new String[]{"1", "2"}) {
            j++;
            continue Label19;
        }

        // startTestCase("run 12");
        new Model().getI();

        // startTestCase("run 13");
        Label20: try {
            new Model().getS();
            break Label20;
        } catch (UnsupportedOperationException e) {
            i = j;
            break Label20;
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            i = 100;
            j = 80;
        }

        // startTestCase("run 14");
        class IntContainer {
            int integer = 0;

            public IntContainer(int initial) {
                super();
                this.integer = initial;
            }

            public IntContainer() {
                this(0);
            }
        }

        // startTestCase("run 15");
        final IntContainer intContainer = new IntContainer();

        Runnable r = new Runnable() {
            int k = 0;

            public void run() {
                intContainer.integer++;
            }
        };
        if (intContainer.integer != 0) {
            throw new IllegalStateException();
        }
        // endTestCase("run 15");
        r.run();

        // startTestCase("run 16");
        r.run();
        if (intContainer.integer == 2)
            ;
        else
            throw new IllegalStateException();
        // endTestCase();
        System.out.print("ENDE");
    }

    private static class Model {
        int i;
        String s;
        
        public int getI() {
            return this.i;
        }
        
        public String getS() {
            throw new UnsupportedOperationException();
        }
    }

    @Target({METHOD, CONSTRUCTOR})
    protected @interface BooleanAnnotation1 {
        boolean value() default false;
    }
}
