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

package org.codecover.instrumentation.java.measurement;

/**
 * An object, that contains a long value.<br>
 * <br>
 * In contrast to {@link Long} the long value is immuteable - especially by
 * using the {@link #increment()} method.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: LongContainer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class LongContainer {
   private long value;

   /**
    * A new {@link LongContainer} with value <b>zero</b>.
    */
   public LongContainer() {
       this.value = 0L;
   }

   /**
    * A new {@link LongContainer} with value <b>value</b>.
    *
    * @param value The initial value of the {@link LongContainer}.
    */
   public LongContainer(long value) {
       this.value = value;
   }

   /**
    * @return The value.
    */
   public long getValue() {
       return this.value;
   }

   /**
    * @param value
    *            The new value of the {@link LongContainer}.
    */
   public void setValue(long value) {
       this.value = value;
   }

   /**
    * Increments the value by <b>one</b>.
    */
   public void increment() {
       this.value++;
   }

   /**
    * Resets the value to <b>zero</b>.
    */
   public void reset() {
       this.value = 0L;
   }
}
