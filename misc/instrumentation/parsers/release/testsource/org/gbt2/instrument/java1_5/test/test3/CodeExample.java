package org.gbt2.instrument.java1_5.test.test3;

import org.gbt2.instrumentation.java15.measurement.Protocol;

/**
 * This is a code example to show how gbt² will instrument the source code.
 * These soure code files will be instrumented by hand.
 * 
 * @author Christoph Müller
 * @version 1.0 - 20.03.2007
 * @see CodeExampleStatementCoverage
 * @see CodeExampleBranchCoverage
 * @see CodeExampleConditionCoverage
 * @see CodeExampleLoopCoverage
 */
public class CodeExample {

    private static final double ALLOWED_MISTAKE = 1E-10;

    private int intervalCount = 10;

    private double left;

    private double right;

    private double exactIntegral;

    private IFunction function;

    public static void main(String[] args) {
        try {
            Protocol.startTestCase("main");
            CodeExample cE = new CodeExample();
            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");

            EApproxMode[] approxModes = EApproxMode.values();
            for (EApproxMode mode : approxModes) {
                cE.approx(mode);

                if (mode.ordinal() >= approxModes.length - 1)
                    continue;

                byte seperatorCount = 0;
                while (seperatorCount < 20) {
                    seperatorCount++;

                    if (seperatorCount < 20)
                        System.out.printf("#");
                    else
                        System.out.printf("%n");
                }
            }

            Protocol.endTestCase("main");

            Protocol.startTestCase("Second Inner Class", "Contains some special statements");
            SecondClassOfFile.getNumber(false);
            Protocol.startTestCase("try catch of CodeExample");
        } catch (RuntimeException e) {
            System.out.printf("RuntimeException occured.");
        } catch (Exception e) {
            System.out.printf("Exception occured.");
        }
    }

    public CodeExample() throws Exception {
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
        for (int i = 1; i < intervalCount; i++) {
            result += function.getValue(left + i * h);
        }
        result += function.getValue(right) / 2.0;
        result *= h;

        return result;
    }

    public static class SinFunction implements IFunction {
        private boolean canBeUsed = true;

        public double getValue(double argument) {
            return Math.sin(argument);
        }

        public boolean canBeUsed() {
            ;
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
}

class SecondClassOfFile {
    static final long ZERO = 0l;

    static int ONE;
    
    static int TWO = 2;

    static {
        ONE = ((int) ZERO) + 1;
    }
    
    static long incrementer = 0;

    int e, f, g = 0;

    int w, x, y;
    
    public static long getNumber(boolean switchNumber) {
        int a, b, c;
        int i, j, k = 0;
        
        while (incrementer <= ONE)
            incrementer++;

        do
            incrementer++;
        while (incrementer <= TWO);

        for (int z = 0; z == Integer.MAX_VALUE; z++)
            incrementer += 1;
        
        doFoo();

        // line comment
        return switchNumber ? ZERO : ONE;
    }

    /**
     * 
     */
    private static void doFoo() {
        int tryBlock = 0;

        try {
            tryBlock = 1;
            throw new Exception("NOOOOOOOOOOO");
        } catch (Exception e) {
            tryBlock = 2;
        } catch (Throwable t) {
            tryBlock = 3;
        }

        try {
            tryBlock = 4;
            throw new Error("NOOOOOOOOOOO!");
        } catch (Throwable t) {
            tryBlock = 5;
        }

        try {
            tryBlock = 6;
            throw new NullPointerException();
        } catch (NullPointerException e) {
            tryBlock = 7;
        } catch (java.lang.Throwable t) {
            tryBlock = 8;
        } finally {
            tryBlock = 9;
        }
        
        return;
    }
}
