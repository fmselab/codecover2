///////////////////////////////////////////////////////////////////////////////
//
// $Id: Main.java 1 2007-12-12 17:37:26Z t-scheller $
// 
///////////////////////////////////////////////////////////////////////////////

/*
 * Packet: 
 * Datei:  Main.java
 */

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;

import org.gbt2.instrumentation.cobol85.parser.CobolParser;
import org.gbt2.instrumentation.cobol85.parser.ParseException;
import org.gbt2.instrumentation.cobol85.syntaxtree.Node;
import org.gbt2.instrumentation.cobol85.visitor.InstrumentVisitor;
import org.gbt2.instrumentation.cobol85.visitor.TreeDumper;
import org.gbt2.instrumentation.cobol85.visitor.Visitor;

/**
 * @author Christoph Müller
 * @version 1.0 - 22.03.2007
 */
public class Main
{
  public static void main(
    String[] args)
  {
    String src = "stuff" + File.separator + "example.cob";
    String targetPath = "stuff" + File.separator + "example-instr.cob";

    try
    {
      File target = new File(targetPath);
      target.getParentFile().mkdirs();
      FileOutputStream oFileOutputStream = new FileOutputStream(targetPath);
      OutputStreamWriter oOutputStreamWriter = new OutputStreamWriter(oFileOutputStream, "UTF-8");
      BufferedWriter oBufferedWriter = new BufferedWriter(oOutputStreamWriter);

      Visitor oVisiter = new InstrumentVisitor(oBufferedWriter);

      CobolParser oParser = new CobolParser(new FileInputStream(src), "UTF-8");
      Node root = oParser.CompilationUnit();

      System.out.printf("------------------------%n");
      root.accept(oVisiter);
      System.out.printf("------------------------%n");

      System.out.printf("Dumping finished!");
    }
    catch (ParseException e)
    {
      e.printStackTrace();
    }
    catch (FileNotFoundException e)
    {
      e.printStackTrace();
    }
    catch (UnsupportedEncodingException e)
    {
      e.printStackTrace();
    }
  }
}
