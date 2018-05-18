///////////////////////////////////////////////////////////////////////////////
//
// $Id: LongContainer.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 17:35:34
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;

/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 *
 */
public class LongContainer {
   public long content;
   
   public LongContainer() {
       this.content = 0L;
   }
   
   public LongContainer(long content) {
       this.content = content;
   }
   
   public void increment() {
       this.content++;
   }
}
