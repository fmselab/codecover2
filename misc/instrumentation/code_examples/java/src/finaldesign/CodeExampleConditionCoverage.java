package finaldesign;

import static org.gbt2.measurement.ConditionCounter.BIT0;
import static org.gbt2.measurement.ConditionCounter.BIT1;
import static org.gbt2.measurement.ConditionCounter.BIT10;
import static org.gbt2.measurement.ConditionCounter.BIT11;
import static org.gbt2.measurement.ConditionCounter.BIT2;
import static org.gbt2.measurement.ConditionCounter.BIT3;
import static org.gbt2.measurement.ConditionCounter.BIT4;
import static org.gbt2.measurement.ConditionCounter.BIT5;
import static org.gbt2.measurement.ConditionCounter.BIT6;
import static org.gbt2.measurement.ConditionCounter.BIT7;
import static org.gbt2.measurement.ConditionCounter.BIT8;
import static org.gbt2.measurement.ConditionCounter.BIT9;

import org.gbt2.measurement.ConditionCounter;
import org.gbt2.measurement.CoverageResultLog;

///////////////////////////////////////////////////////////////////////////////
//
// $Id: CodeExampleConditionCoverage.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 20.03.2007 09:48:50
//
///////////////////////////////////////////////////////////////////////////////

/**
 * This is a code example to show how gbt² will instrument the source code. This
 * soure code file is instrumented by hand. It shows condition coverage.
 * 
 * @author Christoph Müller
 * @version 1.0 - 20.03.2007
 */
public class CodeExampleConditionCoverage {
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
        org.gbt2.measurement.Protocol.startTestCase("CodeExampleConditionCoverage");
        
        try {
            CodeExampleConditionCoverage cE = new CodeExampleConditionCoverage();
            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");

            EApproxMode[] approxModes = EApproxMode.values();
            for (EApproxMode mode : approxModes) {
                cE.approx(mode);

                int conditionLongHelper1;
                if ((((conditionLongHelper1 = 0) == 0) | true) & ((((conditionLongHelper1 |= BIT1) == 0 | true)
                        & (mode.ordinal() >= approxModes.length - 1))
                        && ((conditionLongHelper1 |= BIT0)  == 0 | true)) & Gbt2CoverageCounter.C1.add(conditionLongHelper1))
                    continue;

                byte seperatorCount = 0;
                int conditionLongHelper2;
                while ((((conditionLongHelper2 = 0) == 0) | true) & ((((conditionLongHelper2 |= BIT1) == 0 | true)
                        & (seperatorCount < 20))
                        && ((conditionLongHelper2 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C2.add(conditionLongHelper2)) {
                    seperatorCount++;

                    int conditionLongHelper3;
                    if ((((conditionLongHelper3 = 0) == 0) | true) & ((((conditionLongHelper3 |= BIT1) == 0 | true)
                            & (seperatorCount < 20))
                            && ((conditionLongHelper3 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C3.add(conditionLongHelper3))
                        System.out.printf("#");
                    else
                        System.out.printf("%n");
                }
            }

        } catch (RuntimeException e) {
            System.out.printf("RuntimeException occured.");
        } catch (Exception e) {
            System.out.printf("Exception occured.");
        }
        
        org.gbt2.measurement.Protocol.endTestCase();
    }

    public CodeExampleConditionCoverage() throws Exception {
        this.left = 0.0;
        this.right = Math.PI;
        this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        this.function = new SinFunction();

        int conditionLongHelper4;
        if ((((conditionLongHelper4 = 0) == 0) | true) & ((((conditionLongHelper4 |= BIT1) == 0 | true)
                & (!this.function.canBeUsed()))
                && ((conditionLongHelper4 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C4.add(conditionLongHelper4)) {
            throw new Exception("function not useable");
        }

        int conditionLongHelper5;

        if ((((conditionLongHelper5 = 0) == 0) | true) & (((((conditionLongHelper5 |= BIT5) == 0 | true)
                & this.left == 0.0)
                && ((conditionLongHelper5 |= BIT4) == 0 | true))
        && ((((conditionLongHelper5 |= BIT3) == 0 | true)
                & this.right == Math.PI)
                && ((conditionLongHelper5 |= BIT2) == 0 | true))
        && ((((conditionLongHelper5 |= BIT1) == 0 | true)
                & this.function instanceof SinFunction)
                && ((conditionLongHelper5 |= BIT0) == 0 | true))) & Gbt2CoverageCounter.C5.add(conditionLongHelper5)) {
            this.exactIntegral = 2.0;
        } else {
            int conditionLongHelper6;
            if ((((conditionLongHelper6 = 0) == 0) | true) & ((((conditionLongHelper6 |= BIT11) == 0 | true) &
                    this.left == 0.0
                    && ((conditionLongHelper6 |= BIT10) == 0 | true)
            & ((conditionLongHelper6 |= BIT9) == 0 | true) &
                    this.right == Math.PI / 2
                    && ((conditionLongHelper6 |= BIT8) == 0 | true))
            | ((conditionLongHelper6 |= BIT7) == 0 | true) &
                    this.left == 2 * Math.PI
                    && ((conditionLongHelper6 |= BIT6) == 0 | true)
            & ((conditionLongHelper6 |= BIT5) == 0 | true) &
                    this.right == Math.PI
                    && ((conditionLongHelper6 |= BIT4) == 0 | true)
            || ((conditionLongHelper6 |= BIT4) == 0 | true) &
                    true
                    && ((conditionLongHelper6 |= BIT3) == 0 | true)
            & ((conditionLongHelper6 |= BIT1) == 0 | true) &
                    this.function instanceof SinFunction
                    && ((conditionLongHelper6 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C6.add(conditionLongHelper6)) {
            this.exactIntegral = 1.0;
            } else {
                throw new Exception("unknown correct integral");
            }
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
            int conditionLongHelper7;
            do {
                approxIntegral = calcIntegral(this.left, this.right, intervals,
                        this.function);
                mistake = Math.abs(this.exactIntegral - approxIntegral);

                System.out.printf("   %d intervals%n", new Integer(intervals));
                System.out.printf("   result %14.10f%n", new Double(
                        approxIntegral));
                System.out.printf("   mistake  %13.10G%n", new Double(mistake));

                intervals <<= 1;
            } while ((((conditionLongHelper7 = 0) == 0) | true) & ((((conditionLongHelper7 |= BIT1) == 0 | true)
                    & (mistake > ALLOWED_MISTAKE))
                    && ((conditionLongHelper7 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C7.add(conditionLongHelper7));
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
        int conditionLongHelper8;
        for (int i = 1; ((((conditionLongHelper8 = 0) == 0) | true) & (((conditionLongHelper8 |= BIT1) == 0 | true)
                & (i < intervalCount))
                && ((conditionLongHelper8 |= BIT0) == 0 | true)) & Gbt2CoverageCounter.C8.add(conditionLongHelper8); i++)
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
        public static String className = CodeExampleConditionCoverage.class.getName();

        public static ConditionCounter C1 = new ConditionCounter("C1");
        public static ConditionCounter C2 = new ConditionCounter("C2");
        public static ConditionCounter C3 = new ConditionCounter("C3");
        public static ConditionCounter C4 = new ConditionCounter("C4");
        public static ConditionCounter C5 = new ConditionCounter("C5");
        public static ConditionCounter C6 = new ConditionCounter("C6");
        public static ConditionCounter C7 = new ConditionCounter("C7");
        public static ConditionCounter C8 = new ConditionCounter("C8");

        public void serializeAndReset(CoverageResultLog printer) {
            printer.startFile(className);
            C1.serializeAndReset(printer);
            C2.serializeAndReset(printer);
            C3.serializeAndReset(printer);
            C4.serializeAndReset(printer);
            C5.serializeAndReset(printer);
            C6.serializeAndReset(printer);
            C7.serializeAndReset(printer);
            C8.serializeAndReset(printer);
        }
    }
}
