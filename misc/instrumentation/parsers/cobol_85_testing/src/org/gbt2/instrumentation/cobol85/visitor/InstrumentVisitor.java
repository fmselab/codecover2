///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumentVisitor.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

/*
 * Packet: org.gbt2.instrumentation.cobol85.visitor
 * Datei:  InstrumentVisitor.java
 */
package org.gbt2.instrumentation.cobol85.visitor;

import java.io.BufferedWriter;
import java.io.Writer;
import java.util.Enumeration;

import org.gbt2.instrumentation.cobol85.syntaxtree.NodeToken;

/**
 * @author Christoph Müller
 * @version 1.0 - 23.03.2007
 *
 */
public class InstrumentVisitor
  extends TreeDumper
{

  /**
   *  
   * @param writer
   */
  public InstrumentVisitor(
    Writer writer)
  {
    super(writer);
  }
  
  /**
   *  
   * @param n
   */
  @Override
  protected void printSpecials(
    NodeToken n)
  {
    if ( n.numSpecials() > 0 )
    {
      for ( Enumeration<NodeToken> e = n.specialTokens.elements(); e.hasMoreElements(); ) {
        NodeToken ns = e.nextElement();
        if (ns.tokenImage.startsWith("*>STARTTESTCASE")) {
            out.print("PERFORM STARTTESTCASE");
            String testCaseName = ns.tokenImage.substring(16);
            int index = testCaseName.indexOf('"');
            if (index != -1) {
                testCaseName = testCaseName.substring(index+1);
                index = testCaseName.indexOf('"');
                if (index != -1) {
                    testCaseName = testCaseName.substring(0, index);
                    out.println();
                    out.print("MOVE " + '"' + testCaseName + '"' + " TO COUNT-TEST-CASE-HEADER");
                }
            }
        } else if (ns.tokenImage.startsWith("*>ENDTESTCASE")) {
            out.print("PERFORM ENDTESTCASE");
        } else {
          visit(ns);
        }
      }
    }
  }
}
