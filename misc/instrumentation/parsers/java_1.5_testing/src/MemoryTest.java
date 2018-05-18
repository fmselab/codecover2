import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.LinkedList;
import java.util.List;

import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.syntaxtree.CompilationUnit;
import org.codecover.model.utils.file.FileTool;

import de.ahija.ATTimer;
import de.ahija.ObjectSizer;

/**
 * java/lang/System.java has:
 * 1098 lines, 45.941 Bytes
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: MemoryTest.java 14 2008-05-24 20:55:18Z ahija $)
 */
public class MemoryTest {

    private static final JavaParser parser = new JavaParser("");

    private static List<CompilationUnit> listCU = new LinkedList<CompilationUnit>();

    public static void main(String[] args) {
        try {
            memoryWithGC();
            memoryWithoutGC();
            System.out.println("FINISH");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void memoryWithGC() throws IOException, Exception {
        PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter("stuff/memory-withGC.log")));
    
        File[] toInstrument = new File[]{
                new File("testclasses/jdk/java/lang/System.java"),
                new File("testclasses/jdk/java/awt/Component.java"),
                new File("testclasses/jdk/java/lang/String.java"),
                new File("testclasses/jdk/java/lang/Runtime.java")
        };
    
        writer.printf("%5d %6d %10dd%n",
                new Integer(0),
                new Long(0),
                new Long(ObjectSizer.getMemoryUse()));
    
        ATTimer timer = new ATTimer();
        for (int i = 1; i <= 20; i++) {
            parse(toInstrument[0]);
            parse(toInstrument[1]);
            parse(toInstrument[2]);
            parse(toInstrument[3]);
            writer.printf("%5d %6d %10d%n",
                    new Integer(i),
                    new Long(timer.getDifference()),
                    new Long(ObjectSizer.getMemoryUse()));
            if (i % 1 == 0) {
                ObjectSizer.putOutTheGarbage(1);
            }
            if (i % 2 == 0) {
                System.out.println(i);
            }
            writer.printf("%5d %6d %10d%n",
                    new Integer(i),
                    new Long(timer.getDifference()),
                    new Long(ObjectSizer.getMemoryUse()));
        }
        writer.close();
        timer.printTimer();
    }

    private static void memoryWithoutGC() throws IOException, Exception {
        PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter("stuff/memory-withoutGC.log")));
    
        File[] toInstrument = new File[]{
                new File("testclasses/jdk/java/lang/System.java"),
                new File("testclasses/jdk/java/awt/Component.java"),
                new File("testclasses/jdk/java/lang/String.java"),
                new File("testclasses/jdk/java/lang/Runtime.java")
        };

        writer.printf("%5d %6d %10dd%n",
                new Integer(0),
                new Long(0),
                new Long(ObjectSizer.getMemoryUse()));

        ATTimer timer = new ATTimer();
        for (int i = 1; i <= 20; i++) {
            parse(toInstrument[0]);
            parse(toInstrument[1]);
            parse(toInstrument[2]);
            parse(toInstrument[3]);
            if (i % 2 == 0) {
                System.out.println(i);
            }
            writer.printf("%5d %6d %10d%n",
                    new Integer(i),
                    new Long(timer.getDifference()),
                    new Long(ObjectSizer.getMemoryUse()));
        }
        writer.close();
        timer.printTimer();
    }

    private static void parse(File fileToParse) throws Exception {
        parser.ReInit(FileTool.getContentFromFile(fileToParse, Charset.defaultCharset()));
        CompilationUnit CU = parser.CompilationUnit(); 
//      listCU.add(CU);
    }
}
