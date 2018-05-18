package finaldesign;

import java.util.Vector;

/**
 * @author Christoph Müller
 * @version 1.0 - 14.05.2007
 *
 */
public interface InterfaceExample {
    public Long toLong();

    public class ComplexUser implements InterfaceExample {

        public Boolean toBool() {
            return Boolean.TRUE;
        }

        public String toString() {
            return new String("String");
        }
        
        public Long toLong() {
            return Long.valueOf(0);
        }
        
        public class SimpleUser {
            public static final int i = 0; //new Vector().size();
            
        }
    }
}
