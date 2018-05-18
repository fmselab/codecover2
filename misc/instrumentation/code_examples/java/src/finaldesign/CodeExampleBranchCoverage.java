package finaldesign;

import org.gbt2.measurement.CoverageResultLog;

///////////////////////////////////////////////////////////////////////////////
//
// $Id: CodeExampleBranchCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 20.03.2007 09:48:50
//
///////////////////////////////////////////////////////////////////////////////

/**
 * This is a code example to show how gbt² will instrument the source code. This
 * soure code file is instrumented by hand. It shows branch coverage.
 * 
 * @author Christoph Müller
 * @version 1.0 - 20.03.2007
 * 
 */
public class CodeExampleBranchCoverage {
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
        org.gbt2.measurement.Protocol.startTestCase("CodeExampleBranchCoverage");
        
        try {
            Gbt2CoverageCounter.Try1 = true;

            CodeExampleBranchCoverage cE = new CodeExampleBranchCoverage();
            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");

            EApproxMode[] approxModes = EApproxMode.values();
            for (EApproxMode mode : approxModes) {
                cE.approx(mode);

                if (mode.ordinal() >= approxModes.length - 1) {
                    Gbt2CoverageCounter.B1++;
                    continue;
                }
                else {
                    Gbt2CoverageCounter.B2++;
                }

                byte seperatorCount = 0;
                while (seperatorCount < 20) {
                    seperatorCount++;

                    if (seperatorCount < 20)
                    {
                        Gbt2CoverageCounter.B3++;
                        System.out.printf("#");
                    }
                    else
                    {
                        Gbt2CoverageCounter.B4++;
                        System.out.printf("%n");
                    }
                }
            }

        } catch (RuntimeException e) {
            Gbt2CoverageCounter.Try1 = false;
            Gbt2CoverageCounter.B6++;
            System.out.printf("RuntimeException occured.");
        } catch (Exception e) {
            Gbt2CoverageCounter.Try1 = false;
            Gbt2CoverageCounter.B7++;
            System.out.printf("Exception occured.");
        } catch (Throwable t) {
            Gbt2CoverageCounter.Try1 = false;
            Gbt2CoverageCounter.B8++;
        } finally {
            if (Gbt2CoverageCounter.Try1) {
                Gbt2CoverageCounter.B5++;
            }
        }
        
        org.gbt2.measurement.Protocol.endTestCase();
    }

    public CodeExampleBranchCoverage() throws Exception {
        this.left = 0.0;
        this.right = Math.PI;
        this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        this.function = new SinFunction();

        if (!this.function.canBeUsed()) {
            Gbt2CoverageCounter.B9++;
            throw new Exception("function not useable");
        } else {
            Gbt2CoverageCounter.B10++;
        }
        
        if (this.left == 0.0 && this.right == Math.PI
                && this.function instanceof SinFunction) {
            Gbt2CoverageCounter.B11++;
            this.exactIntegral = 2.0;
        } else if (((this.left == 0.0 & this.right == Math.PI / 2)
                | (this.left == 2 * Math.PI & this.right == Math.PI) || true)
                & this.function instanceof SinFunction) {
            Gbt2CoverageCounter.B12++;
            this.exactIntegral = 1.0;
        } else {
            Gbt2CoverageCounter.B13++;
            throw new Exception("unknown correct integral");
        }
    }

    public void approx(EApproxMode mode) {
        double approxIntegral;
        approxIntegral = 0.0;

        switch (mode.ordinal()) {
        case 0:
            Gbt2CoverageCounter.B14++;
            System.out.printf("> fixed interval count:%n");
            break;
        case 1:
            Gbt2CoverageCounter.B15++;
            System.out.printf("> with given precision%n");
            break;
        default:
            Gbt2CoverageCounter.B16++;
            System.out.printf("> exact result%n");
            break;
        }

        switch (mode) {
        case FIXED_COUNT:
            Gbt2CoverageCounter.B17++;
            System.out.printf("  %d intervals%n", new Integer(
                    this.intervalCount));
            approxIntegral = calcIntegral(this.left, this.right,
                    this.intervalCount, this.function);
            break;
        case PRECISION:
            Gbt2CoverageCounter.B18++;
            int intervals = 1;
            double mistake;
            do {
                approxIntegral = calcIntegral(this.left, this.right, intervals,
                        this.function);
                mistake = Math.abs(this.exactIntegral - approxIntegral);

                System.out.printf("   %d intervals%n", new Integer(intervals));
                System.out.printf("   result %14.10f%n", new Double(
                        approxIntegral));
                System.out.printf("   mistake  %13.10G%n", new Double(mistake));

                intervals <<= 1;
            } while (mistake > ALLOWED_MISTAKE);
            break;
        case EXACT:
            Gbt2CoverageCounter.B19++;
            approxIntegral = this.exactIntegral;
            break;
        default:
            Gbt2CoverageCounter.B20++;
            break;
        }

        System.out.printf("  result: %14.10f%n%n", new Double(approxIntegral));
    }

    private static double calcIntegral(double left, double right,
            int intervalCount, IFunction function) {
        double result = 0.0;
        double h = (right - left) / intervalCount;

        result += function.getValue(left) / 2.0;
        for (int i = 1; i < intervalCount; i++)
            result += function.getValue(left + i * h);
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
        public static String className = CodeExampleBranchCoverage.class.getName();

        public static long B1 = 0L;
        public static long B2 = 0L;
        public static long B3 = 0L;
        public static long B4 = 0L;
        public static long B5 = 0L;
        public static long B6 = 0L;
        public static long B7 = 0L;
        public static long B8 = 0L;
        public static long B9 = 0L;
        public static long B10 = 0L;
        public static long B11 = 0L;
        public static long B12 = 0L;
        public static long B13 = 0L;
        public static long B14 = 0L;
        public static long B15 = 0L;
        public static long B16 = 0L;
        public static long B17 = 0L;
        public static long B18 = 0L;
        public static long B19 = 0L;
        public static long B20 = 0L;
        
        public static boolean Try1 = true;

        public void serializeAndReset(CoverageResultLog printer) {
            printer.startFile(className);
            if (this.B1 > 0) {
                printer.writeCounter("B1", this.B1);
                this.B1 = 0;
            }
            if (this.B2 > 0) {
                printer.writeCounter("B2", this.B2);
                this.B2 = 0;
            }
            if (this.B3 > 0) {
                printer.writeCounter("B3", this.B3);
                this.B3 = 0;
            }
            if (this.B4 > 0) {
                printer.writeCounter("B4", this.B4);
                this.B4 = 0;
            }
            if (this.B5 > 0) {
                printer.writeCounter("B5", this.B5);
                this.B5 = 0;
            }
            if (this.B6 > 0) {
                printer.writeCounter("B6", this.B6);
                this.B6 = 0;
            }
            if (this.B7 > 0) {
                printer.writeCounter("B7", this.B7);
                this.B7 = 0;
            }
            if (this.B8 > 0) {
                printer.writeCounter("B8", this.B8);
                this.B8 = 0;
            }
            if (this.B9 > 0) {
                printer.writeCounter("B9", this.B9);
                this.B9 = 0;
            }
            if (this.B10 > 0) {
                printer.writeCounter("B10", this.B10);
                this.B10 = 0;
            }
            if (this.B11 > 0) {
                printer.writeCounter("B11", this.B11);
                this.B11 = 0;
            }
            if (this.B12 > 0) {
                printer.writeCounter("B12", this.B12);
                this.B12 = 0;
            }
            if (this.B13 > 0) {
                printer.writeCounter("B13", this.B13);
                this.B13 = 0;
            }
            if (this.B14 > 0) {
                printer.writeCounter("B14", this.B14);
                this.B14 = 0;
            }
            if (this.B15 > 0) {
                printer.writeCounter("B15", this.B15);
                this.B15 = 0;
            }
            if (this.B16 > 0) {
                printer.writeCounter("B16", this.B16);
                this.B16 = 0;
            }
            if (this.B17 > 0) {
                printer.writeCounter("B17", this.B17);
                this.B17 = 0;
            }
            if (this.B18 > 0) {
                printer.writeCounter("B18", this.B18);
                this.B18 = 0;
            }
            if (this.B19 > 0) {
                printer.writeCounter("B19", this.B19);
                this.B19 = 0;
            }
            if (this.B20 > 0) {
                printer.writeCounter("B20", this.B20);
                this.B20 = 0;
            }
        }
    }
}
