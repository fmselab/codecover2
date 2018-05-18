///////////////////////////////////////////////////////////////////////////////
//
// $Id: MeasurementHelper.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 17:30:11
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;

/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 * 
 */
public class MeasurementHelper {
    public static void main(String[] args) {
      for (int i = 0; i <= 31; i++)
      {
          System.out.println("public static final int BIT" + i + " = " + Integer.rotateLeft(1, i) + ";");
      }
    }
    
    public static long setBit(long data, int position)
    {
        return data |= Long.rotateLeft(1, position);
    }
    
    public static boolean evaluate(LongContainer condition, int position, boolean result)
    {
        condition.content |= Long.rotateLeft(1, position);
        
        if (result) {
            condition.content |= Long.rotateLeft(1, position + 1);
        }
        return result;
    }
}
