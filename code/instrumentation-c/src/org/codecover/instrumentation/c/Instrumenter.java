package org.codecover.instrumentation.c;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.c.adapter.TokenAdapter;
import org.codecover.instrumentation.c.counter.CounterManager;
import org.codecover.instrumentation.c.manipulators.*;
import org.codecover.instrumentation.c.parser.CParser;
import org.codecover.instrumentation.c.parser.CParserConstants;
import org.codecover.instrumentation.c.parser.DebugCParser;
import org.codecover.instrumentation.c.syntaxtree.TranslationUnit;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.HierarchyLevelType;
import org.codecover.model.mast.SourceFile;
import org.codecover.model.utils.criteria.*;
import org.codecover.model.utils.file.SourceTargetContainer;

import java.io.*;
import java.net.URISyntaxException;
import java.util.*;

public class Instrumenter extends org.codecover.instrumentation.Instrumenter {
    private ArrayList<CounterManager> counterManagers = new ArrayList<CounterManager>();

    private int maxId;

    // Hack to get the File directly and not just a Reader
    private File currentSourceFile;

    @Override
    protected void notifyBefore(SourceTargetContainer job, HierarchyLevelContainer rootHierarchyLevelContainer, MASTBuilder builder, String testSessionContainerUID, Map<String, Object> instrumenterDirectives) throws InstrumentationException {
        currentSourceFile = job.getSource();
    }

    @Override
    protected void instrumentThis(Reader source,
                                  Writer target,
                                  MASTBuilder builder,
                                  SourceFile sourceFile,
                                  HierarchyLevelContainer rootContainer,
                                  String testSessionContainerUID,
                                  Map<String, Object> instrumenterDirectives) throws ParseException, IOException {
        try {
            TranslationUnit translationUnit;

            String[] includeDirs = (String[])instrumenterDirectives.get(InstrumenterDescriptor.IncludeDirs.KEY);
            String[] defines = (String[])instrumenterDirectives.get(InstrumenterDescriptor.Defines.KEY);

            if(false) {
                TokenAdapter adapter = new TokenAdapter(currentSourceFile, Arrays.asList(includeDirs), Arrays.asList(defines), false);
                org.codecover.instrumentation.c.parser.Token t = adapter.getNextToken();
                while (t.kind != CParserConstants.EOF) {
                    target.write(t.image);
                    target.write("\n");
                    t = adapter.getNextToken();
                }
                return;
            }

            if ((Boolean)instrumenterDirectives.get(InstrumenterDescriptor.Debug.KEY)) {
                DebugCParser cParser = new DebugCParser(new TokenAdapter(currentSourceFile, Arrays.asList(includeDirs), Arrays.asList(defines), true));
                for(String type : (String[])instrumenterDirectives.get(InstrumenterDescriptor.Types.KEY)) {
                    cParser.addDecl(type, true);
                }
                translationUnit = cParser.TranslationUnit();
            } else {
                CParser cParser = new CParser(new TokenAdapter(currentSourceFile, Arrays.asList(includeDirs), Arrays.asList(defines), false));
                for(String type : (String[])instrumenterDirectives.get(InstrumenterDescriptor.Types.KEY)) {
                    cParser.addDecl(type, true);
                }
                translationUnit = cParser.TranslationUnit();
            }

            CounterManager cm = new CounterManager(Integer.toString(maxId++), sourceFile.getFileName());

            MastVisitor mastVisitor = new MastVisitor(builder, sourceFile, rootContainer, cm);

            mastVisitor.visit(translationUnit);

            InstrumentationVisitor instrumentationVisitor =
                    new InstrumentationVisitor(target,
                            isCriterionSet(StatementCoverage.getInstance()) ? new DefaultStatementManipulator(cm) : new DummyStatementManipulator(),
                            isCriterionSet(BranchCoverage.getInstance()) ? new DefaultBranchManipulator(cm) : new DummyBranchManipulator(),
                            isCriterionSet(LoopCoverage.getInstance()) ? new DefaultLoopManipulator(cm) : new DummyLoopManipulator(),
                            isCriterionSet(ConditionCoverage.getInstance()) ? new DefaultConditionManipulator(cm) : new DummyConditionManipulator(),
                            isCriterionSet(QMOCoverage.getInstance()) ? new DefaultQMOManipulator(cm) : new DummyQMOManipulator()
                    );


            instrumentationVisitor.visit(translationUnit);

            counterManagers.add(cm);
        } catch(Exception e) {
            throw new ParseException(e.toString());
        }
    }

    @Override
    protected void notifyEnd(File rootFolder,
                             File targetFolder,
                             Collection<SourceTargetContainer> jobs,
                             HierarchyLevelContainer rootHierarchyLevelContainer,
                             MASTBuilder builder, String testSessionContainerUID,
                             Map<String, Object> instrumenterDirectives) throws InstrumentationException {
        try {
            Helper.writeMeasurementFile(counterManagers, new File(targetFolder, "CodeCover.c"), testSessionContainerUID);
            Helper.copyFile(this.getClass().getResourceAsStream("/org/codecover/instrumentation/c/res/tree.h"), new File(targetFolder, "CodeCover.h"));
        } catch (IOException e) {
            throw new InstrumentationException(e);
        }
    }

    @Override
    protected HierarchyLevelType getPackageHierarchyLevelType(MASTBuilder builder) {
        return HierachyLevelTypes.getProgramType(builder);
    }

    @Override
    public boolean allowsFileListInstrumentation() {
        return true;
    }


}
