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
 * @version 1.0 ($Id: ExamplePerson.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class ExamplePerson {
  static {
    CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.ping();
  }


    private String familyName;

    private String givenName;

    private double salary;

    private int accessCount = 0;
  {
    CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[1]++;
  }

    public String getFamilyName() {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[2]++;
        return this.familyName;
    }

    public void setFamilyName(String familyName) {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[3]++;
        this.familyName = familyName;
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[4]++;
    }

    public String getGivenName() {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[5]++;
        return this.givenName;
    }

    public void setGivenName(String givenName) {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[6]++;
        this.givenName = givenName;
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[7]++;
    }

    public double getSalary() {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[8]++;
        return this.salary;
    }

    public void setSalary(double salary) {
        access();
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[9]++;
        this.salary = salary;
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[10]++;
    }
    
    protected void access() {
        this.accessCount++;
CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt.statements[11]++;
    }
}


class CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt extends org.codecover.instrumentation.java.measurement.CounterContainer {

  static {
    org.codecover.instrumentation.java.measurement.ProtocolImpl.getInstance(org.codecover.instrumentation.java.measurement.CoverageResultLogFile.getInstance(null), "e9754141-dfa8-41f5-8e14-02f5795fc346").addObservedContainer(new CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt ());
  }
    public static long[] statements = new long[12];
    public static long[] branches = new long[0];
    public static long[] loops = new long[1];

  public CodeCoverCoverageCounter$5rz4v2um1dnfzq3rjs8lgvfxbgpt () {
    super("org.codecover.classes.ExamplePerson.java");
  }

  public static void ping() {/* nothing to do*/}

  public void reset() {
      for (int i = 1; i <= 11; i++) {
        statements[i] = 0L;
      }
      for (int i = 1; i <= -1; i++) {
        branches[i] = 0L;
      }
      for (int i = 1; i <= 0; i++) {
        loops[i] = 0L;
      }
    }

  public void serializeAndReset(org.codecover.instrumentation.measurement.CoverageCounterLog log) {
    log.startNamedSection("org.codecover.classes.ExamplePerson.java");
      for (int i = 1; i <= 11; i++) {
        if (statements[i] != 0L) {
          log.passCounter("S" + i, statements[i]);
          statements[i] = 0L;
        }
      }
      for (int i = 1; i <= -1; i++) {
        if (branches[i] != 0L) {
          log.passCounter("B"+ i, branches[i]);
          branches[i] = 0L;
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
