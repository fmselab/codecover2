///////////////////////////////////////////////////////////////////////////////
//
// $Id: MaximumFieldCreator.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 24.03.2007 18:21:26
//
///////////////////////////////////////////////////////////////////////////////

package testing;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;

/**
 * This class generates a java file with a lot of fields to get to know, how
 * many fields a class can contain.
 * 
 * @author Christoph Müller
 * @version 1.0 - 24.03.2007
 * 
 */
public class MaximumFieldCreator {
    private static final String TEST_FILE_NAME = "ALotOfFieldsClass";
    private static final String TEST_FILE = "src" + File.separator + 
    "testing" + File.separator + TEST_FILE_NAME + ".java";
    
    private static final int VARIABLE_COUNT = 1000;

    private static final String VARIABLE_PATTERN = "  private long varname%d;%n";

    public static void main(String[] args) {
        try {
            FileOutputStream fileOutputStream = new FileOutputStream(TEST_FILE);
            OutputStreamWriter outputStreamWriter = new OutputStreamWriter(
                    fileOutputStream, Charset.forName("UTF-8"));
            BufferedWriter bufferedWriter = new BufferedWriter(
                    outputStreamWriter);
            
            bufferedWriter.write("package testing;\n");
            bufferedWriter.write("public class " + TEST_FILE_NAME + " {\n");
            for (int i = 1; i <= VARIABLE_COUNT; i++) {
                bufferedWriter.write(String.format(VARIABLE_PATTERN, new Integer(i)));
            }
            
            bufferedWriter.write("\n  public static void main(String[] args) {\n");
            bufferedWriter.write("    System.out.println(\"Variable count: " + VARIABLE_COUNT + "\");\n");
            bufferedWriter.write("  }\n");
            bufferedWriter.write("}\n");
            
            bufferedWriter.flush();
            bufferedWriter.close();
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            e.printStackTrace();
        }
        
        System.out.printf("Finished!%nSize: %14d", new Long(new File(TEST_FILE).length()));
    }
}
