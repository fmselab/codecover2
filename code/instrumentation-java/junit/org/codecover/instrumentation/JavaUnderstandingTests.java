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

package org.codecover.instrumentation;


import java.math.BigInteger;
import java.nio.BufferOverflowException;
import java.nio.BufferUnderflowException;
import java.nio.CharBuffer;
import java.util.Date;

import junit.framework.Assert;
import junit.framework.TestCase;

/**
 * @author Christoph Müller
 * 
 * @version 1.0 ($Id: JavaUnderstandingTests.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class JavaUnderstandingTests extends TestCase {

    public void testStringSame() throws Exception {
        Assert.assertSame("", "");
        Assert.assertSame("".substring(0, 0), "");
        Assert.assertSame("hello", "hello");
        Assert.assertSame(new String().intern(), "");
        Assert.assertSame(new StringBuffer().toString().intern(), "");
        Assert.assertSame(new StringBuilder().toString().intern(), "");
        Assert.assertSame(new String("hello").substring(0, 0).intern(), "");
        Assert.assertSame(new String("hello").intern(), "hello");

        Assert.assertNotSame(new String(), "");
        Assert.assertNotSame(new StringBuffer().toString(), "");
        Assert.assertNotSame(new StringBuilder().toString(), "");
        Assert.assertNotSame(new String("hello").substring(0, 0), "");
        Assert.assertNotSame(new String("hello"), "hello");
    }

    static final int THREAD_COUNT = 2;
    static final int MAX_EACH = (int) 1E7;
    static long[] statements = new long[10];
    static final Object LOCK = new Object();
    
    public void testLongInkrementation() throws Exception {
        long timeStart = System.currentTimeMillis();
        Thread[] manipulators = new Thread[THREAD_COUNT];
    
        for (int i = 0; i < THREAD_COUNT; i++) {
            manipulators[i] = new ManipulationThread();
        }
        for (int i = 0; i < THREAD_COUNT; i++) {
            manipulators[i].start();
        }
    
        WAIT_FOR_FINISH: while (true) {
            for (int i = 0; i < THREAD_COUNT; i++) {
                if (manipulators[i].isAlive()) {
                    break;
                }
                if (i == THREAD_COUNT - 1) {
                    break WAIT_FOR_FINISH;
                }
            }
    
            Thread.sleep(100);
        }
        
        int expected = THREAD_COUNT * MAX_EACH;
//        System.out.println(System.currentTimeMillis() - timeStart);
//        System.out.println(expected + " == " + statements[0]);
        Assert.assertTrue("expected " + expected + ", but was " + statements[0],
                expected == statements[0]);
    }
    
    static class ManipulationThread extends Thread {
        public void run() {
            for (int i = 1; i <= MAX_EACH; i++) {
                increment(0);
            }
        }
    }

    static void increment(int index) {
        synchronized (LOCK) {
            statements[index]++;
        }
    }

    public void testStringFormat() throws Exception {
        String.format("%1$td.%1$tm.%1$tY %1$tH:%1$tM:%1$tS", new Date());
    }

    public void testCharBuffer() throws Exception {
        CharBuffer cb = CharBuffer.allocate(8);
        cb.append("Hello");
        cb.flip();
        Assert.assertEquals("Hello", cb.toString());
        Assert.assertEquals("Hello", cb.toString());
        Assert.assertEquals(8, cb.capacity());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(5, cb.limit());
        // always true
        Assert.assertEquals(cb.limit() - cb.position(), cb.length());

        cb.clear();
        Assert.assertEquals("Hello\u0000\u0000\u0000", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.append("ABCDE");
        cb.flip();
        Assert.assertEquals("ABCDE", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(5, cb.limit());

        cb.append("F");
        cb.flip();
        Assert.assertEquals("F", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(1, cb.limit());

        cb.clear();
        Assert.assertEquals("FBCDE\u0000\u0000\u0000", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.clear();
        cb.append("ABCDEFGH");
        try {
            cb.append("I");
            Assert.fail();
        } catch (BufferOverflowException e) {
            // expected
        }

        cb.clear();
        cb.append("ABCDEFGH");
        cb.flip();
        cb.append("I");
        cb.clear();
        Assert.assertEquals("IBCDEFGH", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.append("Hall");
        cb.append("o");
        cb.flip();
        Assert.assertEquals("Hallo", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(5, cb.limit());

        cb.clear();
        cb.append("12345678");
        cb.flip();
        Assert.assertEquals("12345678", cb.toString());
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.append("ABCD");
        Assert.assertEquals("5678", cb.toString());
        Assert.assertEquals(4, cb.position());
        Assert.assertEquals(8, cb.limit());
        
        cb.clear();
        cb.append("\u0000\u0000\u0000\u0000\u0000\u0000\u0000\u0000");
        cb.clear();
        cb.append("1234");
        cb.mark();
        Assert.assertEquals(4, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.append("56");
        Assert.assertEquals(6, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.reset();
        Assert.assertEquals(4, cb.position());
        Assert.assertEquals(8, cb.limit());

        cb.flip();
        Assert.assertEquals(0, cb.position());
        Assert.assertEquals(4, cb.limit());

        Assert.assertEquals('1', cb.get());
        Assert.assertEquals('2', cb.get());
        Assert.assertEquals('3', cb.get());
        Assert.assertEquals('4', cb.get());
        try {
            Assert.assertEquals('5', cb.get());
            Assert.fail();
        } catch (BufferUnderflowException e) {
            // expected
        }
        cb.limit(6);
        Assert.assertEquals('5', cb.get());
        Assert.assertEquals('6', cb.get());

        Assert.assertEquals(6, cb.position());
        Assert.assertEquals(6, cb.limit());

        try {
            cb.position(7);
            Assert.fail();
        } catch (IllegalArgumentException e) {
            // expected
        }

        cb.limit(4);
        Assert.assertEquals(4, cb.position());
        Assert.assertEquals(4, cb.limit());
    }
    
    public void testPolymorphisms() throws Exception {
        ClassA classA = new ClassA();
        Assert.assertEquals(1, classA.invoke1());
        classA = new ClassB();
        Assert.assertEquals(2, classA.invoke1());
    }

    static class ClassA {
        public int invoke1() {
            return this.invoke2();
        }
        
        public int invoke2() {
            return 1;
        }
    }
    
    static class ClassB extends ClassA {
        @Override
        public int invoke1() {
            return super.invoke1();
        }

        public int invoke2() {
            return 2;
        }
    }

    public void testBigInter() throws Exception {
        BigInteger bigInteger = BigInteger.ONE;
        int[] values = {0 , 1 , 3 , 2 , 0 , 3 , 3 , 2 ,1 , 1 , 1, 0};
        for (int i = 0; i < values.length; i++) {
            bigInteger = bigInteger.shiftLeft(2);
            if ((values[i] & 1) == 1) {
                bigInteger = bigInteger.setBit(0);
            }
            if ((values[i] & 2) == 2) {
                bigInteger = bigInteger.setBit(1);
            }
            System.out.println(bigInteger.toString(2));
        }

        System.out.println(bigInteger.toString());
        int bitCount = (bigInteger.bitLength() -1) / 2;
        Assert.assertEquals(bitCount, values.length);
        for (int i = 0; i < bitCount; i++) {
            int thisNumber = 0;
            if (bigInteger.testBit(i * 2)) {
                thisNumber |= 1;
            }
            if (bigInteger.testBit(i * 2 + 1)) {
                thisNumber |= 2;
            }
            System.out.print(thisNumber + ", ");
            Assert.assertEquals("Index " + i, values[values.length - i - 1], thisNumber);
        }
    }
    
    public void testWrapper() throws Exception {
        int i = 0;
        long l = 0;

        i = (int) ++l;
        
    }
}