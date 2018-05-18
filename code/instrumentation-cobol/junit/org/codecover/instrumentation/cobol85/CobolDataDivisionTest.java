package org.codecover.instrumentation.cobol85;

import static org.codecover.UtilsForTestingCobol.SESSION_CONTAINER;
import static org.codecover.UtilsForTestingCobol.SOURCE;
import static org.codecover.UtilsForTestingCobol.TARGET;
import static org.codecover.UtilsForTestingCobol.handleException;
import static org.codecover.UtilsForTestingCobol.isCompileableCobol;
import static org.codecover.UtilsForTestingCobol.simpleTestSessionContainerTests;

import java.io.File;
import java.nio.charset.Charset;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingCobol;
import org.codecover.instrumentation.DefaultInstrumenterFactory;
import org.codecover.instrumentation.Instrumenter;
import org.codecover.instrumentation.InstrumenterFactory;
import org.codecover.instrumentation.UUIDDirective;
import org.codecover.instrumentation.cobol85.compilerDirectives.BWCompilerDirectivesManipulator;
import org.codecover.instrumentation.exceptions.InstrumentationException;
import org.codecover.model.MASTBuilder;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.utils.criteria.BranchCoverage;
import org.codecover.model.utils.criteria.LoopCoverage;
import org.codecover.model.utils.criteria.StatementCoverage;
import org.codecover.model.utils.file.SourceTargetContainer;

/**
 * @author Stefan Franke
 * @version 1.0 ($Id: CobolDataDivisionTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class CobolDataDivisionTest extends TestCase {
        Instrumenter instrumenter;

        MASTBuilder builder;

        @Override
        protected void setUp() throws Exception {
            this.builder = UtilsForTestingCobol.newMASTBuilder();

            org.codecover.instrumentation.InstrumenterDescriptor descriptor = new InstrumenterDescriptor();
            InstrumenterFactory factory = new DefaultInstrumenterFactory();
            factory.setDescriptor(descriptor);
            factory.setCharset(Charset.forName("UTF-8"));
            factory.setPretendMode(false);

            factory.addCriterion(StatementCoverage.getInstance());
            factory.addCriterion(BranchCoverage.getInstance());
            //factory.addCriterion(ConditionCoverage.getInstance());
            factory.addCriterion(LoopCoverage.getInstance());

            this.instrumenter = factory.getInstrumenter();
        }

        @Override
        protected void tearDown() {
            this.instrumenter = null;
        }

        public void testLF() {
            String srcPath = SOURCE + "DataDivision/LF.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/LF.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testFL() {
            String srcPath = SOURCE + "DataDivision/FL.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/FL.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testFW() {
            String srcPath = SOURCE + "DataDivision/FW.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/FW.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testWF() {
            String srcPath = SOURCE + "DataDivision/WF.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/WF.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testWL() {
            String srcPath = SOURCE + "DataDivision/WL.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/WL.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testLW() {
            String srcPath = SOURCE + "DataDivision/LW.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/LW.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testLWF() {
            String srcPath = SOURCE + "DataDivision/LWF.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/LWF.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }
            
            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testLFW() {
            String srcPath = SOURCE + "DataDivision/LFW.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/LFW.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }

            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testFWL() {
            String srcPath = SOURCE + "DataDivision/FWL.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/FWL.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }

            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testFLW() {
            String srcPath = SOURCE + "DataDivision/FLW.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/FLW.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }

            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testWFL() {
            String srcPath = SOURCE + "DataDivision/WFL.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/WFL.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }

            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        public void testWLF() {
            String srcPath = SOURCE + "DataDivision/WLF.txt";
            File source = new File(srcPath);
            String targetPath = TARGET + "DataDivision/WLF.txt";
            File target = new File(targetPath);
            Date dateBefore = new Date();
            TestSessionContainer testSessionContainer = null;
            Collection<SourceTargetContainer> container = Collections.
            <SourceTargetContainer>singleton(
                    new SourceTargetContainer(source, target));

            try {
              Map<String, Object> instrumenterDirectives = instrumenterDirectives();
              testSessionContainer = this.instrumenter.instrument(source.getParentFile(),
                      target.getParentFile(), container, this.builder, instrumenterDirectives);
            } catch (InstrumentationException e) {
                handleException(e);
            }

            Assert.assertTrue(target.exists());
            simpleTestSessionContainerTests(dateBefore, testSessionContainer, this.instrumenter);
            Assert.assertTrue(isCompileableCobol(target));

            try {
                testSessionContainer.save(SESSION_CONTAINER);
            } catch (Exception e) {
                handleException(e);
            }
            Assert.assertTrue(new File(SESSION_CONTAINER).exists());
        }

        private Map<String, Object> instrumenterDirectives() {
            Map<String, Object> instrumenterDirectives = new HashMap<String, Object>();
              instrumenterDirectives.put("compiler-directives", new BWCompilerDirectivesManipulator());
              instrumenterDirectives.put(UUIDDirective.KEY, UUIDDirective.INSTANCE.getDefaultValue());
            return instrumenterDirectives;
        }
        
}
