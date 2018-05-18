/**
 * Example class to show what coverage information can be highlighted with
 * CodeCover's default metrics.
 * 
 * @author  Johannes Langauf
 * @version 1.0 ($Id: HelloHighlighting.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HelloHighlighting {

    public static void main(String[] args) {

        int three = 4; // basic statement

        /* conditional statements and basic statements */
        try // try: conditional statement cov:
            //   2 catch branches and implicit branch (no or uncaught exception)
        {
            if // if: conditional statement cov: branches: then and (implicit) else
               (three != 3) // (root) term of if with a basic boolean term
            {
                // then branch (no keyword, only a block)
                throw new IllegalArgumentException(); // no basic statement
            } // implicit else branch here
        } catch (IllegalArgumentException e) { // catch: branch
            --three; // basic statement
        } catch (NullPointerException e) { // catch: branch
            System.out.println("exception."); // basic statement
        }
        boolean b = false; // basic statement
        int n = 4; // basic statement
        if // conditional statement
          (b && three == 3) // (root) term of if with two basic boolean terms
        {
            ++n; // basic statement
        } else {
            --n; // basic statement
        }
        switch (n) { // switch: conditional statement, cov: case 3, case 5, (implicit) default
        case 3:  // case 3: branch
            break; // basic statement
        case 5: // case 5: branch
            break; // basic statement
            // implicit default branch here
        }
        
        /* loops */
        do { // do: loop cov: one execution, many executions
            ++three; // basic statement
        } while (b); // (root) term of do with one basic boolean term
        while // while: loop cov: zero executions, one execution, many executions
            (--three > 0 || getFalse()) // (root) term of while two basic boolean terms
        { 
            n = n + three; // basic statement
        }
        for // for: loop cov: zero executions, one execution, many executions
            (int i = 0; i < 4; i++) { // (root) term of for with one basic boolean term
            ++n; // basic statement
        }
    }

    private static boolean getFalse() {
        return false; // no basic statement
    }
}
