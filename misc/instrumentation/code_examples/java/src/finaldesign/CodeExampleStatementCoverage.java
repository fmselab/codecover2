package finaldesign;

import org.gbt2.measurement.CoverageResultLog;

///////////////////////////////////////////////////////////////////////////////
//
// $Id: CodeExampleStatementCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 20.03.2007 09:48:50
//
///////////////////////////////////////////////////////////////////////////////

/**
 * This is a code example to show how gbt² will instrument the source code. This
 * soure code file is instrumented by hand. It shows statement coverage.
 * 
 * @author Christoph Müller
 * @version 1.0 - 20.03.2007
 * 
 */
public class CodeExampleStatementCoverage {
    private static final double ALLOWED_MISTAKE = 1E-10;
    static {Gbt2CoverageCounter.S1++;}

    private int intervalCount = 10;
    {Gbt2CoverageCounter.S2++;}

    private double left;

    private double right;

    private double exactIntegral;

    private IFunction function;

    public static void main(String[] args) {
        org.gbt2.measurement.Protocol.startTestCase("CodeExampleStatementCoverage");
        Gbt2CoverageCounter.S3++;
        
        try {
            CodeExampleStatementCoverage cE = new CodeExampleStatementCoverage();
            Gbt2CoverageCounter.S4++;

            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");
            Gbt2CoverageCounter.S5++;

            EApproxMode[] approxModes = EApproxMode.values();
            Gbt2CoverageCounter.S6++;
            for (EApproxMode mode : approxModes) {
                cE.approx(mode);
                Gbt2CoverageCounter.S7++;

                if (mode.ordinal() >= approxModes.length - 1)
                {
                    Gbt2CoverageCounter.S8++;
                    continue;
                }

                byte seperatorCount = 0;
                Gbt2CoverageCounter.S9++;
                while (seperatorCount < 20) {
                    seperatorCount++;
                    Gbt2CoverageCounter.S10++;

                    if (seperatorCount < 20)
                    {
                        System.out.printf("#");
                        Gbt2CoverageCounter.S11++;
                    }
                    else {
                        System.out.printf("%n");
                        Gbt2CoverageCounter.S12++;
                    }
                }
            }
        } catch (RuntimeException e) {
            System.out.printf("RuntimeException occured.");
            Gbt2CoverageCounter.S13++;
        } catch (Exception e) {
            System.out.printf("Exception occured.");
            Gbt2CoverageCounter.S14++;
        }

        org.gbt2.measurement.Protocol.endTestCase();
        Gbt2CoverageCounter.S15++;
    }

    public CodeExampleStatementCoverage() throws Exception {
        this.left = 0.0;
        Gbt2CoverageCounter.S17++;
        this.right = Math.PI;
        Gbt2CoverageCounter.S18++;
        this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        Gbt2CoverageCounter.S19++;
        this.function = new SinFunction();
        Gbt2CoverageCounter.S20++;

        if (!this.function.canBeUsed()) {
            throw new Exception("function not useable");
        }

        if (this.left == 0.0 && this.right == Math.PI
                && this.function instanceof SinFunction) {
            this.exactIntegral = 2.0;
            Gbt2CoverageCounter.S21++;
        } else if (((this.left == 0.0 & this.right == Math.PI / 2)
                | (this.left == 2 * Math.PI & this.right == Math.PI) || true)
                & this.function instanceof SinFunction) {
            this.exactIntegral = 1.0;
            Gbt2CoverageCounter.S22++;
        } else {
            throw new Exception("unknown correct integral");
        }
    }

    public void approx(EApproxMode mode) {
        double approxIntegral;
        approxIntegral = 0.0;
        Gbt2CoverageCounter.S23++;

        switch (mode.ordinal()) {
        case 0:
            System.out.printf("> fixed interval count:%n");
            Gbt2CoverageCounter.S24++;
            Gbt2CoverageCounter.S25++;
            break;
        case 1:
            System.out.printf("> with given precision%n");
            Gbt2CoverageCounter.S26++;
            Gbt2CoverageCounter.S27++;
            break;
        default:
            System.out.printf("> exact result%n");
            Gbt2CoverageCounter.S28++;
            Gbt2CoverageCounter.S29++;
            break;
        }

        switch (mode) {
        case FIXED_COUNT:
            System.out.printf("  %d intervals%n", new Integer(
                    this.intervalCount));
            Gbt2CoverageCounter.S30++;
            approxIntegral = calcIntegral(this.left, this.right,
                    this.intervalCount, this.function);
            Gbt2CoverageCounter.S31++;
            Gbt2CoverageCounter.S32++;
            break;
        case PRECISION:
            int intervals = 1;
            Gbt2CoverageCounter.S33++;
            double mistake;
            do {
                approxIntegral = calcIntegral(this.left, this.right, intervals,
                        this.function);
                Gbt2CoverageCounter.S34++;
                mistake = Math.abs(this.exactIntegral - approxIntegral);
                Gbt2CoverageCounter.S35++;

                System.out.printf("   %d intervals%n", new Integer(intervals));
                Gbt2CoverageCounter.S36++;
                System.out.printf("   result %14.10f%n", new Double(
                        approxIntegral));
                Gbt2CoverageCounter.S37++;
                System.out.printf("   mistake  %13.10G%n", new Double(mistake));
                Gbt2CoverageCounter.S38++;

                intervals <<= 1;
                Gbt2CoverageCounter.S39++;
            } while (mistake > ALLOWED_MISTAKE);
            Gbt2CoverageCounter.S40++;
            break;
        case EXACT:
            approxIntegral = this.exactIntegral;
            Gbt2CoverageCounter.S41++;
            Gbt2CoverageCounter.S42++;
            break;
        }

        System.out.printf("  result: %14.10f%n%n", new Double(approxIntegral));
        Gbt2CoverageCounter.S43++;
    }

    private static double calcIntegral(double left, double right,
            int intervalCount, IFunction function) {
        double result = 0.0;
        Gbt2CoverageCounter.S44++;
        double h = (right - left) / intervalCount;
        Gbt2CoverageCounter.S45++;

        result += function.getValue(left) / 2.0;
        Gbt2CoverageCounter.S46++;
        for (int i = 1; i < intervalCount; i++) {
            result += function.getValue(left + i * h);
            Gbt2CoverageCounter.S47++;
        }
        result += function.getValue(right) / 2.0;
        Gbt2CoverageCounter.S48++;
        result *= h;
        Gbt2CoverageCounter.S49++;

        return result;
    }

    private static class SinFunction implements IFunction {
        private boolean canBeUsed = true;
        {Gbt2CoverageCounter.S50++;}

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
        static {
            org.gbt2.measurement.Protocol.addObservedContainer(new Gbt2CoverageCounter());
        }

        public static String className = CodeExampleStatementCoverage.class.getName();

        public static long S1 = 0L;
        public static long S2 = 0L;
        public static long S3 = 0L;
        public static long S4 = 0L;
        public static long S5 = 0L;
        public static long S6 = 0L;
        public static long S7 = 0L;
        public static long S8 = 0L;
        public static long S9 = 0L;
        public static long S10 = 0L;
        public static long S11 = 0L;
        public static long S12 = 0L;
        public static long S13 = 0L;
        public static long S14 = 0L;
        public static long S15 = 0L;
        public static long S16 = 0L;
        public static long S17 = 0L;
        public static long S18 = 0L;
        public static long S19 = 0L;
        public static long S20 = 0L;
        public static long S21 = 0L;
        public static long S22 = 0L;
        public static long S23 = 0L;
        public static long S24 = 0L;
        public static long S25 = 0L;
        public static long S26 = 0L;
        public static long S27 = 0L;
        public static long S28 = 0L;
        public static long S29 = 0L;
        public static long S30 = 0L;
        public static long S31 = 0L;
        public static long S32 = 0L;
        public static long S33 = 0L;
        public static long S34 = 0L;
        public static long S35 = 0L;
        public static long S36 = 0L;
        public static long S37 = 0L;
        public static long S38 = 0L;
        public static long S39 = 0L;
        public static long S40 = 0L;
        public static long S41 = 0L;
        public static long S42 = 0L;
        public static long S43 = 0L;
        public static long S44 = 0L;
        public static long S45 = 0L;
        public static long S46 = 0L;
        public static long S47 = 0L;
        public static long S48 = 0L;
        public static long S49 = 0L;
        public static long S50 = 0L;

        public void serializeAndReset(CoverageResultLog printer) {
            printer.startFile(className);
            if (this.S1 > 0) {
                printer.writeCounter("S1", this.S1);
                this.S1 = 0;
            }
            if (this.S2 > 0) {
                printer.writeCounter("S2", this.S2);
                this.S2 = 0;
            }
            if (this.S3 > 0) {
                printer.writeCounter("S3", this.S3);
                this.S3 = 0;
            }
            if (this.S4 > 0) {
                printer.writeCounter("S4", this.S4);
                this.S4 = 0;
            }
            if (this.S5 > 0) {
                printer.writeCounter("S5", this.S5);
                this.S5 = 0;
            }
            if (this.S6 > 0) {
                printer.writeCounter("S6", this.S6);
                this.S6 = 0;
            }
            if (this.S7 > 0) {
                printer.writeCounter("S7", this.S7);
                this.S7 = 0;
            }
            if (this.S8 > 0) {
                printer.writeCounter("S8", this.S8);
                this.S8 = 0;
            }
            if (this.S9 > 0) {
                printer.writeCounter("S9", this.S9);
                this.S9 = 0;
            }
            if (this.S10 > 0) {
                printer.writeCounter("S10", this.S10);
                this.S10 = 0;
            }
            if (this.S11 > 0) {
                printer.writeCounter("S11", this.S11);
                this.S11 = 0;
            }
            if (this.S12 > 0) {
                printer.writeCounter("S12", this.S12);
                this.S12 = 0;
            }
            if (this.S13 > 0) {
                printer.writeCounter("S13", this.S13);
                this.S13 = 0;
            }
            if (this.S14 > 0) {
                printer.writeCounter("S14", this.S14);
                this.S14 = 0;
            }
            if (this.S15 > 0) {
                printer.writeCounter("S15", this.S15);
                this.S15 = 0;
            }
            if (this.S16 > 0) {
                printer.writeCounter("S16", this.S16);
                this.S16 = 0;
            }
            if (this.S17 > 0) {
                printer.writeCounter("S17", this.S17);
                this.S17 = 0;
            }
            if (this.S18 > 0) {
                printer.writeCounter("S18", this.S18);
                this.S18 = 0;
            }
            if (this.S19 > 0) {
                printer.writeCounter("S19", this.S19);
                this.S19 = 0;
            }
            if (this.S20 > 0) {
                printer.writeCounter("S20", this.S20);
                this.S20 = 0;
            }
            if (this.S21 > 0) {
                printer.writeCounter("S21", this.S21);
                this.S21 = 0;
            }
            if (this.S22 > 0) {
                printer.writeCounter("S22", this.S22);
                this.S22 = 0;
            }
            if (this.S23 > 0) {
                printer.writeCounter("S23", this.S23);
                this.S23 = 0;
            }
            if (this.S24 > 0) {
                printer.writeCounter("S24", this.S24);
                this.S24 = 0;
            }
            if (this.S25 > 0) {
                printer.writeCounter("S25", this.S25);
                this.S25 = 0;
            }
            if (this.S26 > 0) {
                printer.writeCounter("S26", this.S26);
                this.S26 = 0;
            }
            if (this.S27 > 0) {
                printer.writeCounter("S27", this.S27);
                this.S27 = 0;
            }
            if (this.S28 > 0) {
                printer.writeCounter("S28", this.S28);
                this.S28 = 0;
            }
            if (this.S29 > 0) {
                printer.writeCounter("S29", this.S29);
                this.S29 = 0;
            }
            if (this.S30 > 0) {
                printer.writeCounter("S30", this.S30);
                this.S30 = 0;
            }
            if (this.S31 > 0) {
                printer.writeCounter("S31", this.S31);
                this.S31 = 0;
            }
            if (this.S32 > 0) {
                printer.writeCounter("S32", this.S32);
                this.S32 = 0;
            }
            if (this.S33 > 0) {
                printer.writeCounter("S33", this.S33);
                this.S33 = 0;
            }
            if (this.S34 > 0) {
                printer.writeCounter("S34", this.S34);
                this.S34 = 0;
            }
            if (this.S35 > 0) {
                printer.writeCounter("S35", this.S35);
                this.S35 = 0;
            }
            if (this.S36 > 0) {
                printer.writeCounter("S36", this.S36);
                this.S36 = 0;
            }
            if (this.S37 > 0) {
                printer.writeCounter("S37", this.S37);
                this.S37 = 0;
            }
            if (this.S38 > 0) {
                printer.writeCounter("S38", this.S38);
                this.S38 = 0;
            }
            if (this.S39 > 0) {
                printer.writeCounter("S39", this.S39);
                this.S39 = 0;
            }
            if (this.S40 > 0) {
                printer.writeCounter("S40", this.S40);
                this.S40 = 0;
            }
            if (this.S41 > 0) {
                printer.writeCounter("S41", this.S41);
                this.S41 = 0;
            }
            if (this.S42 > 0) {
                printer.writeCounter("S42", this.S42);
                this.S42 = 0;
            }
            if (this.S43 > 0) {
                printer.writeCounter("S43", this.S43);
                this.S43 = 0;
            }
            if (this.S44 > 0) {
                printer.writeCounter("S44", this.S44);
                this.S44 = 0;
            }
            if (this.S45 > 0) {
                printer.writeCounter("S45", this.S45);
                this.S45 = 0;
            }
            if (this.S46 > 0) {
                printer.writeCounter("S46", this.S46);
                this.S46 = 0;
            }
            if (this.S47 > 0) {
                printer.writeCounter("S47", this.S47);
                this.S47 = 0;
            }
            if (this.S48 > 0) {
                printer.writeCounter("S48", this.S48);
                this.S48 = 0;
            }
            if (this.S49 > 0) {
                printer.writeCounter("S49", this.S49);
                this.S49 = 0;
            }
            if (this.S50 > 0) {
                printer.writeCounter("S50", this.S50);
                this.S50 = 0;
            }
        }
    }
}