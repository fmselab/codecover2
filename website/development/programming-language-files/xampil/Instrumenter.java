package org.codecover.instrumentation.xampil;

/**
 * This class is the main class of the Xampil instrumenter. It is derived from
 * the {@link org.codecover.instrumentation.Instrumenter} and overrides the
 * instrumentThis method to controls the instrumentation process.<br>
 * <br>
 * <b>Attention</b> dont use {@link #instrument(File, File, Collection, MASTBuilder, Map)}
 * with a Collection with more than one file. This will cause an
 * {@link InstrumentationRuntimeException}.
 *
 * @author Stefan Franke
 *
 * @see #allowsFileListInstrumentation()
 */
class Instrumenter extends org.codecover.instrumentation.Instrumenter {

    /**
     * Constructs the Xampil {@link Instrumenter}.
     */
    public Instrumenter() {
        super();
    }

    @Override
    protected void instrumentThis(Reader source,
            Writer target,
            MASTBuilder database,
            SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException {
        SimpleCharStream simpleCharStream = new SimpleCharStream(source);
        XampilParser xampilParser = new XampilParser(simpleCharStream);
        CompilationUnit compilationUnit = xampilParser.CompilationUnit();
        PrintWriter targetPrintWriter = new PrintWriter(target);
        InstrumentationVisitor instrumentationVisitor = new InstrumentationVisitor(
            targetPrintWriter, database, sourceFile, hierarchyLevelContainer,
            testSessionContainerUID);
        instrumentationVisitor.visit(compilationUnit);
        targetPrintWriter.flush();
    }

    @Override
    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder database) {
        return HierarchyLevelTypes.getSourceFileType(database);
    }

    @Override
    public boolean allowsFileListInstrumentation() {
        return false;
    }
}