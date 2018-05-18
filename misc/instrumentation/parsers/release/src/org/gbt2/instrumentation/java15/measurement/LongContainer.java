///////////////////////////////////////////////////////////////////////////////
//
// $Id: LongContainer.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 17:35:34
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

/**
 * An object, that contains a long value.<br>
 * <br>
 * In contrast to {@link Long} the long value is immuteable - especially by
 * using the {@link #increment()} method.
 * 
 * @author Christoph Müller
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
