package testing;

import java.util.Collections;
import java.util.Vector;

public class CompilerTests {
    
    public static void main(String[] args) {

        if (new Vector().isEmpty()) ;
        if (new Vector<String>().isEmpty()) ;
        if (Collections.emptySet().isEmpty()) ;
        if (Collections.<String>emptySet().isEmpty()) ;

        if ((new Vector().isEmpty())) ;
        if ((new Vector<String>().isEmpty())) ;
        if ((Collections.emptySet().isEmpty())) ;
        if ((Collections.<String>emptySet().isEmpty()));
    }
}
