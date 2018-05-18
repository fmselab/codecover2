package org.codecover.instrumentation.test;

import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;
import java.util.zip.ZipEntry;
import java.util.zip.ZipException;
import java.util.zip.ZipFile;

import org.codecover.CodeCoverInfo;
import org.codecover.UtilsForTestingJava;
import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.java.measurement.MeasurementTimer;
import org.codecover.instrumentation.java15.HierarchyLevelTypeProvider;
import org.codecover.instrumentation.java15.InstrumenterDescriptor;
import org.codecover.instrumentation.java15.InstrumenterTestHook;
import org.codecover.instrumentation.java15.UnicodeEscapeTest;
import org.codecover.instrumentation.java15.parser.JavaParser;
import org.codecover.instrumentation.java15.parser.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.utils.file.FileTool;

/**
 * Diese Klasse parst alle Quellen in {@link #SOURCE_ARRAY}. Dies ist ein Test, ob
 * die Quellen evtl. Java-konforme Konstrukte enthalten, die der Parser aber nicht versteht.
 * 
 * Im {@link #SOURCE_ARRAY} können sowohl jar, zip als auch Ordner angegeben werden.
 * 
 * Diese werden alle geparst und evtl. {@link ParseException} weisen auf Fehler im Parser hin.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: ParserTest.java 14 2008-05-24 20:55:18Z ahija $)
 */
public class ParserTest {
    private static final String[] SOURCE_ARRAY = {"testclasses",
                                                  "../../../../code/",
                                                  "C:/Program Files/Eclipse3.3.1.1/plugins/"};

    private static final Set<String> EXCLUDED_FILES = new HashSet<String>();
    static {
        EXCLUDED_FILES.add("org/apache/commons/el/ImplicitObjects.java");
        EXCLUDED_FILES.add("org/apache/commons/el/parser/ELParser.java");
        EXCLUDED_FILES.add("org.eclipse.pde.ui.templates_3.3.0.v20070608-1300.jar!templates_");
    }

    private static final List<JavaTestSource> usedSources = new Vector<JavaTestSource>();

    private static final MASTBuilder MAST_BUILDER = UtilsForTestingJava.newMASTBuilder();
    private static final HierarchyLevelTypeProvider HL_PROVIDER = new HierarchyLevelTypeProvider(MAST_BUILDER);
    private static final HierarchyLevelType HL_TYPE = HL_PROVIDER.getClassType();
    private static final Map<String, Object> DEFAULT_DIRECTIVES = new InstrumenterDescriptor().getDefaultDirectiveValues();

    public static void main(String[] args) {
        try {
            for (String arg : SOURCE_ARRAY) {
                int oldSize = usedSources.size();
                File thisFile = new File(arg).getCanonicalFile();
                if (!thisFile.exists()) {
                    throw new FileNotFoundException(thisFile.getPath());
                }
                handleFile(thisFile);
                System.out.printf("Found %,7d sources in %s.%n",
                        new Integer(usedSources.size() - oldSize), thisFile.getPath());
            }

            System.out.printf("Found %,7d sources in sum.%n",
                    new Integer(usedSources.size()));
            System.out.println("Parsing...");

            parse();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void handleFile(File file) throws IOException {
        if (file.isFile()) {
            String extension = FileTool.getExtension(file); 
            if (extension.equalsIgnoreCase("java")) {
                addJavaFile(file);
            } else if (extension.equalsIgnoreCase("zip") ||
                       extension.equalsIgnoreCase("jar")) {
                addZipSources(file);
            } else {
                throw new IllegalArgumentException("Extension " + extension + " unknown.");
            }
        } else if (file.isDirectory()) {
            File[] children = file.listFiles(UseableFileFilter.INSTANCE);
            if (children != null) {
                for (File thisChild : children) {
                    handleFile(thisChild.getCanonicalFile());
                }
            }
        }
    }

    private static void addJavaFile(File file) throws IOException {
        Charset charset = decideCharset(file);
        addJavaSource(new FileTestSource(file.getPath(), charset));
    }

    private static void addZipSources(File thisFile) throws ZipException, IOException {
        ZipFile zipFile = new ZipFile(thisFile);
        Enumeration<? extends ZipEntry> entries = zipFile.entries();
        while (entries.hasMoreElements()) {
            ZipEntry thisEntry = entries.nextElement();
            if (!thisEntry.isDirectory()) {
                String extension = FileTool.getExtension(thisEntry.getName()); 
                if (extension.equalsIgnoreCase("java")) {
                    Charset charset = decideCharset(zipFile, thisEntry);
                    addJavaSource(new ZipTestSource(zipFile, thisEntry, charset));
                }
            }
        }
    }

    private static void parse() throws IOException {
        int counter = 0;
        long contentLength = 0l;
        MeasurementTimer timer = new MeasurementTimer();
        timer.setPhaseStart();
        for (JavaTestSource thisTestSource : usedSources) {
            counter++;
            contentLength += thisTestSource.getSize();
            parse(thisTestSource);
            if ((counter % 100) == 0) {
                timer.setPhaseEnd();
                System.out.printf("%,8d | %,11dMB | %s%n",
                        new Integer(counter), new Long(contentLength), timer.getPhaseDuration());
                timer.setPhaseStart();
            }
        }
        timer.setOverallEnd();
        System.out.printf("Finished Parsing%n%,8d | %,11d B | %s%n",
                new Integer(counter), new Long(contentLength), timer.getOverallDuration());
    }

    private static void parse(JavaTestSource thisTestSource) throws IOException {
        Reader reader = null;
        JavaParser parser = null;
        StringWriter targetWriter = null;
        try {
            reader = thisTestSource.getReader();
            String fileContent = FileTool.getContentFromReader(reader);
            targetWriter = new StringWriter(fileContent.length() * 5);

            HierarchyLevelContainer hlContainer = new HierarchyLevelContainer(
                    "SourceFile", HL_TYPE, HL_TYPE);
            InstrumenterTestHook instrumenter = InstrumenterTestHook.INSTANCE;
            instrumenter.instrumentThis(new StringReader(fileContent),
                                        targetWriter,
                                        MAST_BUILDER,
                                        MAST_BUILDER.createSourceFile(thisTestSource.getName(), fileContent),
                                        hlContainer,
                                        "ID",
                                        DEFAULT_DIRECTIVES);

            parser = new JavaParser(targetWriter.toString());
            parser.CompilationUnit();
        } catch (Exception e) {
            System.out.println(thisTestSource.getFullName() + " (" +
                    thisTestSource.getCharset() + ")");
            if (parser == null) {
                System.out.print("EXCEPTION IN INSTRUMENTATION:");
            } else {
                System.out.print("EXCEPTION IN PARSING INSTRUMENTATION:");
            }
            System.out.println("\n" + e.getMessage());
        }
    }

    private static void addJavaSource(JavaTestSource javaTestSource) {
        for (String excluded : EXCLUDED_FILES) {
            if (javaTestSource.getFullName().contains(excluded)) {
                return;
            }
        }
        usedSources.add(javaTestSource);
    }
    
    private static Charset decideCharset(File file) {
        String path = file.getPath().replace('\\', '/');
        Charset charset = Charset.defaultCharset();
        if (path.contains("/trunk/code/")) {
            charset = CodeCoverInfo.CODE_FILE_CHARSET;
        }
        if (path.endsWith("/trunk/code/instrumentation-java/testsource/org/codecover/instrumentation/java15/test/test8/CodeExample.java")) {
            charset = UnicodeEscapeTest.CODE_EXAMPLE_ESCAPED_CHARSET;
        }
        return charset;
    }

    private static Charset decideCharset(ZipFile zipFile, ZipEntry entry) {
        String path = zipFile.getName().replace('\\', '/');
        String pathEntry = entry.getName().replace('\\', '/');
        Charset charset = Charset.defaultCharset();
        if (path.contains("testsource.zip") ||
            path.contains("plugins/org.apache.")) {
            charset = CodeCoverInfo.CODE_FILE_CHARSET;
        }
        return charset;
    }

    private static class UseableFileFilter implements FileFilter {
        static final FileFilter INSTANCE = new UseableFileFilter();

        public boolean accept(File file) {
            if (file.isDirectory()) {
                return true;
            } else if (file.isFile()) {
                String extension = FileTool.getExtension(file);
                return extension.equalsIgnoreCase("java") ||
                extension.equalsIgnoreCase("zip") ||
                extension.equalsIgnoreCase("jar");
            }
            return false;
        }
    }
}
