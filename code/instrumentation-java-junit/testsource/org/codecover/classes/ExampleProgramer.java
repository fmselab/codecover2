/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This program and the accompanying materials           *
 * are made available under the terms of the Eclipse Public License v1.0      *
 * which accompanies this distribution, and is available at                   *
 * http://www.eclipse.org/legal/epl-v10.html                                  *
 ******************************************************************************/

package org.codecover.classes;

/**
 * Usage: To be instrumented for TestRunner tests by build.xml, target <code>instrument-test</code>
 * 
 * @author Christoph Müller 
 *
 * @version 1.0 ($Id: ExampleProgramer.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExampleProgramer extends ExamplePerson {
  static {
    CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.ping();
  }

    
    public ExampleProgramer() {
        super.access();
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[1]++;
        super.setFamilyName("");
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[2]++;
        super.setGivenName("");
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[3]++;
    }

    public void setFamilyName(String familyName) {
        super.access();
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[4]++;
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[5]++;
int CodeCoverConditionCoverageHelper_C1;
        if (((((CodeCoverConditionCoverageHelper_C1 = 0) == 0) | true) & (
(((CodeCoverConditionCoverageHelper_C1 |= (2)) == 0 | true) &
 ((familyName.equals("Müller")) && 
  ((CodeCoverConditionCoverageHelper_C1 |= (1)) == 0 | true)))
)) & CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.conditionCounters[1].incrementCounterOfBitMask(CodeCoverConditionCoverageHelper_C1, 1)) {
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.branches[1]++;
            familyName = "Mueller";
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[6]++;

        } else {
  CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.branches[2]++;}
        super.setFamilyName(familyName);
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[7]++;
    }
    
    public void setSalary(double salary) {
        super.access();
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[8]++;
        super.setSalary(salary * 1.1);
CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5.statements[9]++;
    }
}


class CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5 extends org.codecover.instrumentation.java.measurement.CounterContainer {

  static {
    org.codecover.instrumentation.java.measurement.ProtocolImpl.getInstance(org.codecover.instrumentation.java.measurement.CoverageResultLogFile.getInstance(null), "e9754141-dfa8-41f5-8e14-02f5795fc346").addObservedContainer(new CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5 ());
  }
    public static long[] statements = new long[10];
    public static long[] branches = new long[3];

    public static final org.codecover.instrumentation.java.measurement.ConditionCounter[] conditionCounters = new org.codecover.instrumentation.java.measurement.ConditionCounter[2];
    static {
      conditionCounters[1] = new org.codecover.instrumentation.java.measurement.SmallOneConditionCounter("org.codecover.classes.ExampleProgramer.java", "C1");
    }
    public static long[] loops = new long[1];

  public CodeCoverCoverageCounter$1lpewl7mxtvpe8fxo5k67mo5eigr2g0v5 () {
    super("org.codecover.classes.ExampleProgramer.java");
  }

  public static void ping() {/* nothing to do*/}

  public void reset() {
      for (int i = 1; i <= 9; i++) {
        statements[i] = 0L;
      }
      for (int i = 1; i <= 2; i++) {
        branches[i] = 0L;
      }
      for (int i = 1; i <= 1; i++) {
        if (conditionCounters[i] != null) {
          conditionCounters[i].reset();
        }
      }
      for (int i = 1; i <= 0; i++) {
        loops[i] = 0L;
      }
    }

  public void serializeAndReset(org.codecover.instrumentation.measurement.CoverageCounterLog log) {
    log.startNamedSection("org.codecover.classes.ExampleProgramer.java");
      for (int i = 1; i <= 9; i++) {
        if (statements[i] != 0L) {
          log.passCounter("S" + i, statements[i]);
          statements[i] = 0L;
        }
      }
      for (int i = 1; i <= 2; i++) {
        if (branches[i] != 0L) {
          log.passCounter("B"+ i, branches[i]);
          branches[i] = 0L;
        }
      }
      for (int i = 1; i <= 1; i++) {
        if (conditionCounters[i] != null) {
          conditionCounters[i].serializeAndReset(log);
        }
      }
      for (int i = 1; i <= 0; i++) {
        if (loops[i * 3 - 2] != 0L) {
          log.passCounter("L" + i + "-0", loops[i * 3 - 2]);
          loops[i * 3 - 2] = 0L;
        }
        if ( loops[i * 3 - 1] != 0L) {
          log.passCounter("L" + i + "-1", loops[i * 3 - 1]);
          loops[i * 3 - 1] = 0L;
        }
        if ( loops[i * 3] != 0L) {
          log.passCounter("L" + i + "-2", loops[i * 3]);
          loops[i * 3] = 0L;
        }
      }
  }
}
