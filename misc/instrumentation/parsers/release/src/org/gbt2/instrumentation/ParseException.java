///////////////////////////////////////////////////////////////////////////////
//
// $Id: ParseException.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 26.03.2007 22:56:40
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

/**
 * This class is a parent class for ParseExceptions. It is needed for throwing
 * parse exception out of the abstract method 
 * instrumentThis(Reader source, Writer target) in class 
 * org.gbt2.instrumentation.Instrumenter. 
 * 
 * The parser's ParseException class must be a child of this class.
 * 
 * @author Stefan Franke
 * @version 1.0 - 26.03.2007
 *
 */
public class ParseException extends Exception {
    
    public ParseException() {
        super();
    }
    
    public ParseException(String s) {
        super(s);
    }
    
    public String getMessage() {
        return super.getMessage();
    }
}
