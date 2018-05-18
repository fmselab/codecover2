import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Iterator;

import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.model.utils.file.DefaultIgnores;
import org.codecover.model.utils.file.DirectoryScanner;
import org.codecover.model.utils.file.FileTool;
import org.codecover.model.utils.file.listener.IncludedFileFoundListener;

import de.ahija.ATTimer;
import de.ahija.ObjectSizer;

/**
 * under testclasses lies the jdk1.5.0_11<br>
 * about 6555 files, 65.906.383 Bytes<br>
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: JDKTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class JDKTest {

    private static final JavaParser parser = new JavaParser("");

    public static void main(String[] args) {
        try {
            ATTimer timer = new ATTimer();
            IncludedFileFoundListener iFFL = new IncludedFileFoundListener();
            DirectoryScanner directoryScanner = new DirectoryScanner();
            directoryScanner.addIgnorePatterns(DefaultIgnores.getIgnorePatterns());
            directoryScanner.addIncludePattern("**\\*.java");

            timer.getLastDifference();
            directoryScanner.scan(new File("testclasses/jdk"), iFFL);
            timer.printTimer();

            Collection<File> files = iFFL.getIncludedFiles(); 
            System.out.println("#files: " + files.size());

            PrintWriter writer = new PrintWriter(new BufferedWriter(new FileWriter("stuff/jdk.log")));

            Iterator<File> iterator = files.iterator();
            writer.printf("%5d %6d %10d %10d%n",
                    new Integer(0),
                    new Long(timer.getDifference()),
                    new Long(0),
                    new Long(ObjectSizer.getMemoryUse()));

            for (int i = 1; iterator.hasNext(); i++) {
                File thisFile = iterator.next();
                parse(thisFile);
                writer.printf("%5d %6d %10d %10d%n",
                        new Integer(i),
                        new Long(timer.getDifference()),
                        new Long(thisFile.length()),
                        new Long(ObjectSizer.getMemoryUse()));

                if (i % 100 == 0) {
                    // ObjectSizer.putOutTheGarbage(1);
                    System.out.println(i);
                }
            }
            writer.close();

            timer.printTimer();
            System.out.println("FINISH");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void parse(File fileToParse) throws Exception {
        parser.ReInit(FileTool.getContentFromFile(fileToParse, Charset.defaultCharset()));
        parser.CompilationUnit();
    }
}
