package finaldesign;

import org.gbt2.measurement.CoverageResultLog;

///////////////////////////////////////////////////////////////////////////////
//
// $Id: CodeExampleLoopCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 20.03.2007 09:48:50
//
///////////////////////////////////////////////////////////////////////////////

/**
 * This is a code example to show how gbt² will instrument the source code. This
 * soure code file is instrumented by hand. It shows loop coverage.
 * 
 * @author Christoph Müller
 * @version 1.0 - 20.03.2007
 * 
 */
public class CodeExampleLoopCoverage {
    static {
        org.gbt2.measurement.Protocol.addObservedContainer(new Gbt2CoverageCounter());
    }

    private static final double ALLOWED_MISTAKE = 1E-10;

    private int intervalCount = 10;

    private double left;

    private double right;

    private double exactIntegral;

    private IFunction function;

    public static void main(String[] args) {
        org.gbt2.measurement.Protocol.startTestCase("CodeExampleLoopCoverage");
        
        try {
            CodeExampleLoopCoverage cE = new CodeExampleLoopCoverage();
            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");

            EApproxMode[] approxModes = EApproxMode.values();
            
            long gbt2CoverageCounterLoop1Temp = 0;
            try {
                for (EApproxMode mode : approxModes) {
                    gbt2CoverageCounterLoop1Temp++;
    
                    cE.approx(mode);
    
                    if (mode.ordinal() >= approxModes.length - 1)
                        continue;
    
                    byte seperatorCount = 0;
                    //TODO move to inner class
                    long gbt2CoverageCounterLoop2Temp = 0;
                    try {
                        while (seperatorCount < 20) {
                            gbt2CoverageCounterLoop2Temp++;
                            seperatorCount++;
        
                            if (seperatorCount < 20)
                                System.out.printf("#");
                            else
                                System.out.printf("%n");
                        }
                    } finally {
                        if (gbt2CoverageCounterLoop2Temp == 0) {
                            Gbt2CoverageCounter.L2_0++;
                        } else if (gbt2CoverageCounterLoop2Temp == 1) {
                            Gbt2CoverageCounter.L2_1++;
                        } else {
                            Gbt2CoverageCounter.L2_2++;
                        }
                    }
                }
            } finally {
                if (gbt2CoverageCounterLoop1Temp == 0) {
                    Gbt2CoverageCounter.L1_0++;
                } else if (gbt2CoverageCounterLoop1Temp == 1) {
                    Gbt2CoverageCounter.L1_1++;
                } else {
                    Gbt2CoverageCounter.L1_2++;
                }
            }
        } catch (RuntimeException e) {
            System.out.printf("RuntimeException occured.");
        } catch (Exception e) {
            System.out.printf("Exception occured.");
        }
        
        org.gbt2.measurement.Protocol.endTestCase();
    }

    public CodeExampleLoopCoverage() throws Exception {
        this.left = 0.0;
        this.right = Math.PI;
        this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        this.function = new SinFunction();

        if (!this.function.canBeUsed()) {
            throw new Exception("function not useable");
        }
        
        if (this.left == 0.0 && this.right == Math.PI
                && this.function instanceof SinFunction) {
            this.exactIntegral = 2.0;
        } else if (((this.left == 0.0 & this.right == Math.PI / 2)
                | (this.left == 2 * Math.PI & this.right == Math.PI) || true)
                & this.function instanceof SinFunction) {
            this.exactIntegral = 1.0;
        } else {
            throw new Exception("unknown correct integral");
        }
    }

    public void approx(EApproxMode mode) {
        double approxIntegral;
        approxIntegral = 0.0;

        switch (mode.ordinal()) {
        case 0:
            System.out.printf("> fixed interval count:%n");
            break;
        case 1:
            System.out.printf("> with given precision%n");
            break;
        default:
            System.out.printf("> exact result%n");
            break;
        }

        switch (mode) {
        case FIXED_COUNT:
            System.out.printf("  %d intervals%n", new Integer(
                    this.intervalCount));
            approxIntegral = calcIntegral(this.left, this.right,
                    this.intervalCount, this.function);
            break;
        case PRECISION:
            int intervals = 1;
            double mistake;
            long gbt2CoverageCounterLoop3Temp = 0;
            try {
                do {
                    gbt2CoverageCounterLoop3Temp++;
                    approxIntegral = calcIntegral(this.left, this.right, intervals,
                            this.function);
                    mistake = Math.abs(this.exactIntegral - approxIntegral);
    
                    System.out.printf("   %d intervals%n", new Integer(intervals));
                    System.out.printf("   result %14.10f%n", new Double(
                            approxIntegral));
                    System.out.printf("   mistake  %13.10G%n", new Double(mistake));
    
                    intervals <<= 1;
                } while (mistake > ALLOWED_MISTAKE);
            } finally {
                if (gbt2CoverageCounterLoop3Temp == 0) {
                    Gbt2CoverageCounter.L3_0++;
                } else if (gbt2CoverageCounterLoop3Temp == 1) {
                    Gbt2CoverageCounter.L3_1++;
                } else {
                    Gbt2CoverageCounter.L3_2++;
                }
            }
            break;
        case EXACT:
            approxIntegral = this.exactIntegral;
            break;
        }

        System.out.printf("  result: %14.10f%n%n", new Double(approxIntegral));
    }

    private static double calcIntegral(double left, double right,
            int intervalCount, IFunction function) {
        double result = 0.0;
        double h = (right - left) / intervalCount;

        result += function.getValue(left) / 2.0;
        long gbt2CoverageCounterLoop4Temp = 0;
        try {
            for (int i = 1; i < intervalCount; i++) {
                gbt2CoverageCounterLoop4Temp++;
                result += function.getValue(left + i * h);
            }
        } finally
        {
            if (gbt2CoverageCounterLoop4Temp == 0) {
                Gbt2CoverageCounter.L4_0++;
            } else if (gbt2CoverageCounterLoop4Temp == 1) {
                Gbt2CoverageCounter.L4_1++;
            } else {
                Gbt2CoverageCounter.L4_2++;
            }
        }
        result += function.getValue(right) / 2.0;
        result *= h;

        return result;
    }

    private static class SinFunction implements IFunction {
        private boolean canBeUsed = true;

        public double getValue(double argument) {
            return Math.sin(argument);
        }

        public boolean canBeUsed() {
            return canBeUsed;
        }
    }

    public static interface IFunction {
        public double getValue(double argument);

        public boolean canBeUsed();
    }

    public enum EApproxMode {
        FIXED_COUNT, PRECISION, EXACT
    }
    
    private static class Gbt2CoverageCounter implements org.gbt2.measurement.CounterContainer {
        public static String className = CodeExampleLoopCoverage.class.getName();

        public static long L1_0 = 0L;
        public static long L1_1 = 0L;
        public static long L1_2 = 0L;
        public static long L2_0 = 0L;
        public static long L2_1 = 0L;
        public static long L2_2 = 0L;
        public static long L3_0 = 0L;
        public static long L3_1 = 0L;
        public static long L3_2 = 0L;
        public static long L4_0 = 0L;
        public static long L4_1 = 0L;
        public static long L4_2 = 0L;

        public void serializeAndReset(CoverageResultLog printer) {
            printer.startFile(className);
            if (this.L1_0 > 0) {
                printer.writeCounter("L1_0", this.L1_0);
                this.L1_0 = 0;
            }
            if (this.L1_1 > 0) {
                printer.writeCounter("L1_1", this.L1_1);
                this.L1_1 = 0;
            }
            if (this.L1_2 > 0) {
                printer.writeCounter("L1_2", this.L1_2);
                this.L1_2 = 0;
            }
            if (this.L2_0 > 0) {
                printer.writeCounter("L2_0", this.L2_0);
                this.L2_0 = 0;
            }
            if (this.L2_1 > 0) {
                printer.writeCounter("L2_1", this.L2_1);
                this.L2_1 = 0;
            }
            if (this.L2_2 > 0) {
                printer.writeCounter("L2_2", this.L2_2);
                this.L2_2 = 0;
            }
            if (this.L3_0 > 0) {
                printer.writeCounter("L3_0", this.L3_0);
                this.L3_0 = 0;
            }
            if (this.L3_1 > 0) {
                printer.writeCounter("L3_1", this.L3_1);
                this.L3_1 = 0;
            }
            if (this.L3_2 > 0) {
                printer.writeCounter("L3_2", this.L3_2);
                this.L3_2 = 0;
            }
            if (this.L4_0 > 0) {
                printer.writeCounter("L4_0", this.L4_0);
                this.L4_0 = 0;
            }
            if (this.L4_1 > 0) {
                printer.writeCounter("L4_1", this.L4_1);
                this.L4_1 = 0;
            }
            if (this.L4_2 > 0) {
                printer.writeCounter("L4_2", this.L4_2);
                this.L4_2 = 0;
            }
        }
    }
}