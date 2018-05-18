///////////////////////////////////////////////////////////////////////////////
//
// $Id: IntegerContainer.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 22.03.2007 08:53:25
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;

/**
 * @author Christoph Müller
 * @version 1.0 - 22.03.2007
 *
 */
public class IntegerContainer {
  public int content;
  
  public IntegerContainer(int content) {
      this.content = content;
  }
  
  public IntegerContainer() {
      this(0);
  }
  
  public void increment() {
      this.content++;
  }
}
