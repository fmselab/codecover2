package org.codecover.instrumentation.java15;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.Map;

import org.codecover.instrumentation.HierarchyLevelContainer;
import org.codecover.instrumentation.exceptions.ParseException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InstrumenterTestHook.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterTestHook extends Instrumenter {

    public static final InstrumenterTestHook INSTANCE = new InstrumenterTestHook();
    
    @Override
    public void instrumentThis(Reader source, Writer target,
            MASTBuilder builder, SourceFile sourceFile,
            HierarchyLevelContainer hierarchyLevelContainer,
            String testSessionContainerUID,
            Map<String, Object> instrumenterDirectives) throws ParseException,
            IOException {
        super.instrumentThis(source, target, builder, sourceFile,
                hierarchyLevelContainer, testSessionContainerUID,
                instrumenterDirectives);
    }
}
