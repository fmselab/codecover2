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

public class LoopTest
{
  public static void main(String[] args) {
      int counter;
      counter = 0;
      while (counter <= -1) ;
      counter = 0;
      while (counter <= 0) counter++;
      counter = 0;
      while (counter <= 1) counter++;
      counter = 0;
      while (counter <= 100) counter++;

      counter = 0;
      do {
          counter++;
      } while (counter <= 0);
      counter = 0;
      do {
          counter++;
      } while (counter <= 1);
      counter = 0;
      do {
          counter++;
      } while (counter <= 100);

      for (counter = 0; counter <= -1; counter++) ;
      for (counter = 0; counter <= 0; counter++) ;
      for (counter = 0; counter <= 1; counter++) ;
      for (counter = 0; counter <= 100; counter++) ;

      System.out.print(args[0]);
  }
}