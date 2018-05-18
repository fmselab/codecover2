///////////////////////////////////////////////////////////////////////////////
//
// $Id: Gbt2CoverageCounterProvider.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 16:25:29
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;


/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 * 
 */
public class Gbt2CoverageCounterProvider {
    public static void main(String[] args) {
       String className; 
       int statementCount = 0;
       int branchCount = 0;
       int conditionCount = 0;
       int loopCount = 0;

       className = finaldesign.CodeExampleStatementCoverage.class.getSimpleName();
       statementCount = 52;
//       className = finaldesign.CodeExampleBranchCoverage.class.getSimpleName();
//       branchCount = 19;
//       className = finaldesign.CodeExampleConditionCoverage.class.getSimpleName();
//       conditionCount = 8;
//       className = finaldesign.CodeExampleLoopCoverage.class.getSimpleName();
//       loopCount = 4;
       
       System.out.println("    private static class Gbt2CoverageCounter implements org.gbt2.measurement.CounterContainer {");
       System.out.println("        public static String className = " + className + ".class.getName();");

       System.out.println();
       for (int i = 1; i <= statementCount; i++) {
           System.out.println("        public static long S" + i + " = 0L;");
       }
       for (int i = 1; i <= branchCount; i++) {
           System.out.println("        public static long B" + i + " = 0L;");
       }
       for (int i = 1; i <= conditionCount; i++) {
           System.out.println("        public static ConditionCounter C" + i + " = new ConditionCounter(\"C" + i + "\");");
       }
       for (int i = 1; i <= loopCount; i++) {
           for (int j = 0; j <= 2; j++) {
               System.out.println("        public static long L" + i + "_" + j + " = 0L;");
           }
       }
       
       System.out.println();
       
       System.out.println("        public void serializeAndReset(CoverageResultLog printer) {");
       System.out.println("            printer.startFile(className);");
       for (int i = 1; i <= statementCount; i++) {
           System.out.println("            if (this.S" + i + " > 0) {");
           System.out.println("                printer.writeCounter(\"S" + i + "\", this.S" + i + ");");
           System.out.println("                this.S" + i + " = 0;");
           System.out.println("            }");
       }
       for (int i = 1; i <= branchCount; i++) {
           System.out.println("            if (this.B" + i + " > 0) {");
           System.out.println("                printer.writeCounter(\"B" + i + "\", this.B" + i + ");");
           System.out.println("                this.B" + i + " = 0;");
           System.out.println("            }");
       }
       for (int i = 1; i <= conditionCount; i++) {
           System.out.println("            C" + i + ".serializeAndReset(printer);");
       }
       for (int i = 1; i <= loopCount; i++) {
           for (int j = 0; j <= 2; j++) {
               System.out.println("            if (this.L" + i + "_" + j + " > 0) {");
               System.out.println("                printer.writeCounter(\"L" + i + "_" + j + "\", this.L" + i + "_" + j + ");");
               System.out.println("                this.L" + i + "_" + j + " = 0;");
               System.out.println("            }");
           }
       }
       System.out.println("        }");
       System.out.println("    }");
    }
}
