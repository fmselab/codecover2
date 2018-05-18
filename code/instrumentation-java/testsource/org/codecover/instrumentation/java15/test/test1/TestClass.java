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

package org.codecover.instrumentation.java15.test.test1;

import java.util.Iterator;

import org.codecover.instrumentation.java15.test.test2.AVLTree;
import org.codecover.instrumentation.java15.test.test5.ComplexInterface;
import org.codecover.instrumentation.java15.test.test6.ComplexEnum;
import org.codecover.instrumentation.java15.test.test7.ComplexAnnotation;

public class TestClass
{
  public static void main(String[] args) {
      AVLTree<String> avl = new AVLTree<String>(String.class);
      int base = Integer.MAX_VALUE / 4;
      for (int i = 0; i <= 200; i++) {
          base += 1928374;
          String thisString = Integer.toString(base);
          thisString = thisString.substring(thisString.length() - 4, thisString.length());
          avl.add(thisString);
      }

      Iterator<String> iterator;

      System.out.println("Up>>>>>>>>>");
      // startTestCase("UP");
      iterator = avl.iteratorUpAll();
      while (iterator.hasNext()) {
          System.out.println(iterator.next());
      }
      System.out.println("Up<<<<<<<<<");

      System.out.println("Down>>>>>>>>>");
      // startTestCase("DOWN", "Down\nAll");
      iterator = avl.iteratorDownAll();
      while (iterator.hasNext()) {
          System.out.println(iterator.next());
      }
      // endTestCase("DOWN");
      System.out.println("Down<<<<<<<<<");
      
      System.out.println("Between>>>>>>>>>");
      // startTestCase("BETWEEN");
      iterator = avl.iteratorUpBetween("1111", "8888");
      while (iterator.hasNext()) {
          System.out.println(iterator.next());
      }
      System.out.println("Between<<<<<<<<<");

      ComplexInterface compInt = new ComplexInterface.ComplexUser();
      System.out.println(compInt.toBool());
      System.out.println(compInt.toLong());

      ComplexEnum.ClassInComplexEnum compEnum = new ComplexEnum.ClassInComplexEnum();
      System.out.println(compEnum.getFavourite());

      ComplexAnnotation.ClassInComplexAnnotation compAnn = new ComplexAnnotation.ClassInComplexAnnotation();
      compAnn.annotated();

      new TestClass();
  }

  public static void MAIN(String[] args) {
    // just for name testing
  }

  public static void MaiN(String[] args) {
      // just for name testing
  }

  public TestClass()
  {    
    System.out.println("Juhuu");
    System.out.flush();

    doSomething();
  }

  private void doSomething()
  {
    int a = 45;

    int b = a * a;

    double c = Math.pow(a, b);

    --a;

    a = a >\u003e 1;
    a = a \u003e> 1;
    a = a >>\u003e 1;
    a = a >\u003e> 1;
    a = a \u003e>> 1;

    while (a == 45) {
        throw new RuntimeException();
    }

    b = b * a;
    
    String s = new String("Ökologische Äcker sind übermütig und weiß.").trim();
  }

  protected void blubb() throws Exception {
      do {
          throw new Exception();
      } while (true);
  }
} // This is a final comment