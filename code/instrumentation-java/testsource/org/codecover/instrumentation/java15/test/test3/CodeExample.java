package org.codecover.instrumentation.java15.test.test3;
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

import static java.lang.annotation.ElementType.CONSTRUCTOR;
import static java.lang.annotation.ElementType.FIELD;
import static java.lang.annotation.ElementType.LOCAL_VARIABLE;
import static java.lang.annotation.ElementType.METHOD;

import java.io.Serializable;
import java.lang.annotation.Target;
import java.util.Locale;
import java.util.Vector;
import java.util.regex.Pattern;
import java.awt.event.ActionListener;
import java.awt.event.ActionEvent;

import org.codecover.instrumentation.java.measurement.Protocol;

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
            Protocol.startTestCase("my Name is \"main\"");
            Locale.setDefault(Locale.GERMANY);
            CodeExample cE = new CodeExample();
            System.out.printf("Integral of sin function -"
                    + "approximated with trapezium rule%n%n");

            EApproxMode[] approxModes = EApproxMode.values();
            for (EApproxMode mode : approxModes) {
                cE.approx(mode);

                if ((mode.ordinal() >= approxModes.length - 1) || (1 == 0))
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

            Protocol.endTestCase("my Name is \"main\"");

            Protocol.startTestCase("Second Inner Class", "Contains some\n\"special statements\"");
            SecondClassOfFile.getNumber(false);
            Protocol.startTestCase("try\ncatch of CodeExample");

            AdvancedFolderBackupOrder.conditionTestMethod();
        } catch (RuntimeException e) {
            System.out.printf("RuntimeException occured:%n%s%n", e.getMessage());
            e.printStackTrace(System.out);
        } catch (Exception e) {
            System.out.printf("Exception occured:%n%s%n", e.getMessage());
            e.printStackTrace(System.out);
        }
    }

    public CodeExample() throws Exception {
        this.left = 0.0;
        this.right = Math.PI;
        this.intervalCount = ((this.right - this.left) > Math.PI ? 20 : 12);
        this.function = new SinFunction();

        if (!this.function.canBeUsed()) {
            throw new Exception("function not usable!");
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
            int intervalCount;
            double intervalCountDouble = 1.0;
            final double factor = 1.05;

            double mistake;
            do {
                intervalCount = (int) intervalCountDouble;
                approxIntegral = calcIntegral(this.left, this.right, intervalCount,
                        this.function);
                mistake = Math.abs(this.exactIntegral - approxIntegral);

                System.out.printf("   %d intervals%n", new Integer(intervalCount));
                System.out.printf("   result %14.10f%n", new Double(
                        approxIntegral));
                System.out.printf("   mistake  %13.10G%n", new Double(mistake));

                intervalCountDouble *= factor;
            } while (mistake > ALLOWED_MISTAKE);
            break;
        case EXACT:
            approxIntegral = this.exactIntegral;
            break;
        }

        System.out.printf("  result: %14.10f%n%n", new Double(approxIntegral));
    }

    @org.codecover.instrumentation.java15.test.test3.SecondClassOfFile.NewAnnotation1(true)
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

    private interface IFunction {
        public static final int CONSTANT = Integer.valueOf(0);

        public double getValue(double argument);

        public boolean canBeUsed();
    }

    protected enum EApproxMode {
        FIXED_COUNT, PRECISION, EXACT
    }
}

final class SecondClassOfFile extends Object implements Serializable, Runnable {
    static final long ZERO = 0l;

    static int ONE;

    static int TWO = 2;

    static {
        ONE = ((int) ZERO) + 1;
    }

    static long incrementer = 0;

    int e, f, g = 0;

    int w, x, y;

    int isTrueCounter = 1;

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
        testBooleans();

        switch (2) {
        case 1: break;
        case 2:
        }

        switch (3) {
        case 1: break;
        case 2:
        case 3:
        default:
        }

        // line comment
        return switchNumber ? ZERO : ONE;
    }

    private static void testBooleans() {
        boolean b1;
        boolean b2;
        boolean b3;

        if (true) {} else {Asserts.fail();}
        if (false) {Asserts.fail();}

        b1 = true;
        if (b1) {} else {Asserts.fail();}
        b1 = false;
        if (b1) {Asserts.fail();}

        b1 = true;
        if (!b1) {Asserts.fail();}
        b1 = false;
        if (!b1) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 | b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 | b2) {} else {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 | b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 | b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 || b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 || b2) {} else {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 || b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 || b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 ^ b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 ^ b2) {} else {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 ^ b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 ^ b2) {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 & b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 & b2) {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 & b2) {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 & b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 && b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 && b2) {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 && b2) {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 && b2) {} else {Asserts.fail();}

        b1 = false; b2 = false; b3 = false;
        if (b1 ? b2 : b3) {Asserts.fail();}
        b1 = false; b2 = false; b3 = true;
        if (b1 ? b2 : b3) {} else {Asserts.fail();}
        b1 = false; b2 = true; b3 = false;
        if (b1 ? b2 : b3) {Asserts.fail();}
        b1 = false; b2 = true; b3 = true;
        if (b1 ? b2 : b3) {} else {Asserts.fail();}
        b1 = true; b2 = false; b3 = false;
        if (b1 ? b2 : b3) {Asserts.fail();}
        b1 = true; b2 = false; b3 = true;
        if (b1 ? b2 : b3) {Asserts.fail();}
        b1 = true; b2 = true; b3 = false;
        if (b1 ? b2 : b3) {} else {Asserts.fail();}
        b1 = true; b2 = true; b3 = true;
        if (b1 ? b2 : b3) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 = b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 = b2) {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 = b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 = b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 |= b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 |= b2) {} else {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 |= b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 |= b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 &= b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 &= b2) {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 &= b2) {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 &= b2) {} else {Asserts.fail();}

        b1 = false; b2 = false;
        if (b1 ^= b2) {Asserts.fail();}
        b1 = true; b2 = false;
        if (b1 ^= b2) {} else {Asserts.fail();}
        b1 = false; b2 = true;
        if (b1 ^= b2) {} else {Asserts.fail();}
        b1 = true; b2 = true;
        if (b1 ^= b2) {Asserts.fail();}

        if (new Vector<java.util.zip.ZipInputStream>().isEmpty()) {} else {Asserts.fail();}
        /*
         * TODO uncomment
         * if (Collections.<StringBuilder>emptySet().isEmpty()) {} else {fail();}
         * if (Collections.<String>singletonList("Singleton").isEmpty()) {fail();}
         */
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

        new SecondClassOfFile().run();

        InnerInterface.ECarSelection.ClassInEnum.instance.returnIsMedium();

        // some UNICODE tests
        if ('\u00a9' != '©') {
            Asserts.fail('\u00a9' + " != " + '©');
        }

        String orig = "[©] привет";
        String escaped = "\u005b\u00a9\u005D\u0020\u043F\u0440\u0438\u0432\u0435\u0442";
        if (!orig.equals(escaped)) {
            Asserts.fail("!\"" + orig + "\".equals(\"" + escaped + "\")");
        }

        String returnValue = getE();

        return;
    }

    public static String getE() {
        return Integer.toString(CodeExample.class.getName().length(), 31);
    }

    public void run() {
        Object org = new String("das org macht Probleme");

        class RunnerClass {
            private int count = 0;

            public RunnerClass() {
                this.count = 0;
            }

            public RunnerClass(int i) {
                this();
                this.count = 0 * i;
            }

            public boolean getBoolean() {
                return ++this.count < 32;
            }
        }

        final class RunnerClass2 extends RunnerClass {
            public RunnerClass2() {
                super(2);
            }

            @Override
            public boolean getBoolean() {
                return super.getBoolean();
            }
        }

        ActionListener actionListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                    int i = 0;
                    i++;
            }
        };

        actionListener.actionPerformed(new ActionEvent(this, 0, null) {
            @Override
            public long getWhen() {
                long time = System.currentTimeMillis();
                time *= time;
                time *= time;
                time *= time;
                return time;
            }
        });

        Object de = new Object();

        // test some labels:
        Label1 : Label2 : de = new String("de");
        AssertLabel: assert (de != null);
        Label3 : {;}
        Label4 : ;
        Label5 : de = new String("de");
        Label6 : switch (1) { default : Label6_1 : break;}
        Label7 : if (de == null) ;
        Label8 : while (de == null) {
            de = new String("de");
            Label8_1 : continue;
        }
        Label9 : do de = new String("de"); while (de == null);
        Label10 : for (;;) break Label10;
        Label11 : for (int i = 0; i != 0 ; i = 0) break Label11;
        Label12 : for (Object o : (new Vector<Object>())) break Label12;

        Label13: try {
            Label14 : throw new NullPointerException();
        } catch(NullPointerException e) {
            ;
        } finally {
            ;
        }

        Label15 : synchronized (de) {
            RunnerClass runnerClass = new RunnerClass2();
            while (runnerClass.getBoolean()) {
                dooBlubb();
            }            
        }

        OuterFor : for (int i = 1; i <= 100; i++) {
            for (int j = 1; j <= i; j++) {
                if (j == i) {
                    continue OuterFor;
                }
            }            
        }

        if (1 == "".length()) {
            Label19 : throw new RuntimeException("Böse");
        }
        Label20 : return;
    }

    private void dooBlubb() {
        if (isTrue(1) | isTrue(2) | isTrue(3) | isTrue(4) | isTrue(5)
                | isTrue(6) || isTrue(7) | isTrue(8) | isTrue(9) | isTrue(10)
                | isTrue(11) | isTrue(12) | isTrue(13) || isTrue(14) | isTrue(15)
                | isTrue(16)) {
            ;
        }
        if (isTrue(1) | isTrue(2) || isTrue(3) | isTrue(4) | isTrue(5)
                | isTrue(6) | isTrue(7) | isTrue(8) | isTrue(9) | isTrue(10)
                | isTrue(11) | isTrue(12) | isTrue(13) || isTrue(14) | isTrue(15)
                | isTrue(16) || isTrue(17)) {
            ;
        }
    }

    private boolean isTrue(int bit) {
        if (this.isTrueCounter <= 16) {
            this.isTrueCounter++;

            return bit > 16;
        }
        this.isTrueCounter += this.isTrueCounter << 7;

        Label0 : return Integer.bitCount(this.isTrueCounter & bit) != 0;
    }

    /**
     * I want to test a class in an enum in an interface in a class. ;-)
     * 
     * @author Christoph Müller 
     */
    interface InnerInterface {
        final static int lengthToMoon = 384400;

        public String returnIsMedium();

        @Deprecated
        public enum ECarSelection {
            Audi, BMW, Mercedes, Volkswagen, Porsche, Opel, Seat, Fiat, Ferrari, AstonMartin, Peugeot, Renault, Toyota, Citroen, Ford, Honda, Hyundai, Jaguar, Kia, Mazda, Skoda, Smart, Suzuki, Volvo;

            @SuppressWarnings("all")
            static class ClassInEnum implements InnerInterface {
                static InnerInterface instance = new ClassInEnum();

                protected String isMedium = "false";

                {
                    if (InnerInterface.lengthToMoon == 384400) {
                        this.isMedium = Boolean.toString(true);
                    }
                }

                public ClassInEnum() {
                    // nothing to do.
                }

                public String returnIsMedium() {
                    return this.isMedium;
                }
            }

            @Target({LOCAL_VARIABLE})
            @interface NewAnnotation2 {
                boolean value();
            }
        }

        @Target({METHOD, CONSTRUCTOR})
        abstract @interface NewAnnotation3 {
            boolean value();
        }
    }

    @Target({METHOD, CONSTRUCTOR})
    protected @interface NewAnnotation1 {
        boolean value();

        @Target({FIELD})
        @interface NewAnnotation4 {
            boolean value();
        }
    }
}

class AdvancedFolderBackupOrder {
    public static void conditionTestMethod() {
        Protocol.startTestCase("AdvancedFolderBackupOrder condition test",
                "I have added some code to test the accept method and get" +
        "100% condition coverage");
        new AdvancedFolderBackupOrder().testConditionTestFolder();
        Protocol.endTestCase("AdvancedFolderBackupOrder condition test");
    }

    /**
     * Falls die Blacklist alles verbieten möchte.
     * 
     * @see #allowNoFolders()
     * @see #allowNoFiles()
     */
    public static final Pattern REJECT_PATTERN = Pattern.compile("",
            Pattern.DOTALL);

    private Pattern m_oFolderWhitelistFind;

    private Pattern m_oFolderWhitelistMatch;

    private Pattern m_oFolderBlacklistFind;

    private Pattern m_oFolderBlacklistMatch;

    /**
     * Alternativer Konstruktor<br>
     * <br>
     * Rekursion wird verwendet.
     * 
     * @see #AdvancedFolderBackupOrder(boolean)
     */
    public AdvancedFolderBackupOrder() {
        this.m_oFolderWhitelistFind = null;
        this.m_oFolderWhitelistMatch = null;
        this.m_oFolderBlacklistFind = null;
        this.m_oFolderBlacklistMatch = null;
    }

    /**
     * @return Das Find-Pattern der Whitelist für Ordner.
     */
    public Pattern getFolderWhitelistFindPattern() {
        return this.m_oFolderWhitelistFind;
    }

    /**
     * @param p_oFolderWhitelist
     *            Das neue Find-Pattern der Whitelist für Ordner.
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder setFolderWhitelistFindPattern(
            Pattern p_oFolderWhitelist) {
        this.m_oFolderWhitelistFind = p_oFolderWhitelist;
        return this;
    }

    /**
     * @return Das Match-Pattern der Whitelist für Ordner.
     */
    public Pattern getFolderWhitelistMatchPattern() {
        return this.m_oFolderWhitelistMatch;
    }

    /**
     * @param p_oFolderWhitelist
     *            Das neue Match-Pattern der Whitelist für Ordner.
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder setFolderWhitelistMatchPattern(
            Pattern p_oFolderWhitelist) {
        this.m_oFolderWhitelistMatch = p_oFolderWhitelist;
        return this;
    }

    /**
     * @return Das Find-Pattern der Blacklist für Ordner.
     */
    public Pattern getFolderBlacklistFindPattern() {
        return this.m_oFolderBlacklistFind;
    }

    /**
     * @param p_oFolderBlacklist
     *            Das neue Find-Pattern der Whitelist für Ordner.
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder setFolderBlacklistFindPattern(
            Pattern p_oFolderBlacklist) {
        this.m_oFolderBlacklistFind = p_oFolderBlacklist;
        return this;
    }

    /**
     * @return Das Match-Pattern der Blacklist für Ordner.
     */
    public Pattern getFolderBlacklistMatchPattern() {
        return this.m_oFolderBlacklistMatch;
    }

    /**
     * @param p_oFolderBlacklist
     *            Das neue Match-Pattern der Whitelist für Ordner.
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder setFolderBlacklistMatchPattern(
            Pattern p_oFolderBlacklist) {
        this.m_oFolderBlacklistMatch = p_oFolderBlacklist;
        return this;
    }

    /**
     * Setzt die Whitelist für Ordner auf alles und leert Blacklist.
     * 
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder allowAllFolders() {
        this.m_oFolderWhitelistFind = null;
        this.m_oFolderWhitelistMatch = null;
        this.m_oFolderBlacklistFind = null;
        this.m_oFolderBlacklistMatch = null;
        return this;
    }

    /**
     * Leert die White- und Blacklist für Ordner.
     * 
     * @return Dieses {@link AdvancedFolderBackupOrder} Objekt.
     */
    public AdvancedFolderBackupOrder allowNoFolders() {
        this.m_oFolderWhitelistFind = null;
        this.m_oFolderWhitelistMatch = null;
        this.m_oFolderBlacklistFind = REJECT_PATTERN;
        this.m_oFolderBlacklistMatch = null;
        return this;
    }

    public boolean accept(String strName) {
        AdvancedFolderBackupOrder oAFOBO = AdvancedFolderBackupOrder.this;

        Pattern whiteFind;
        Pattern whiteMatch;
        Pattern blackFind;
        Pattern blackMatch;
        whiteFind = oAFOBO.getFolderWhitelistFindPattern();
        whiteMatch = oAFOBO.getFolderWhitelistMatchPattern();
        blackFind = oAFOBO.getFolderBlacklistFindPattern();
        blackMatch = oAFOBO.getFolderBlacklistMatchPattern();

        // Der Name wird akzeptiert
        if (((whiteFind == null && whiteMatch == null) ||
             (whiteFind != null && whiteFind.matcher(strName).find()) ||
             (whiteMatch != null && whiteMatch.matcher(strName).matches())) &&
            // und Name wird nicht abgelehnt!
            !(blackFind != null && blackFind.matcher(strName).find()) &&
            !(blackMatch != null && blackMatch.matcher(strName).matches())) {
            return true;
        } else {
            return false;
        }
    }

    private void setFolderPatterns(String whiteFind, String whiteMatch,
            String blackFind, String blackMatch) {
        setFolderWhitelistFindPattern(whiteFind == null ? (Pattern) null
                : Pattern.compile(whiteFind));
        setFolderWhitelistMatchPattern(whiteMatch == null ? (Pattern) null
                : Pattern.compile(whiteMatch));
        setFolderBlacklistFindPattern(blackFind == null ? (Pattern) null
                : Pattern.compile(blackFind));
        setFolderBlacklistMatchPattern(blackMatch == null ? (Pattern) null
                : Pattern.compile(blackMatch));
    }

    private boolean acceptsFolder() {
        return accept("folder 1");
    }

    public void testConditionTestFolder() {
        // we want condition coverage for accept(String strName)
        // the 10101 are related to the expected term evaluation
        // the ++ marks the term to be covered
        // JUnit test in org.codecover.bigtest.instrumentationJavaAllInstrumentationTest#checkCoverabelItemsTerm78
        setFolderPatterns("NOT", null, null, null);       //10001110100000000000
        Asserts.assertFalse(acceptsFolder());             //++
        setFolderPatterns(null, null, null, null);        //11110000000010001000
        Asserts.assertTrue(acceptsFolder());                

        setFolderPatterns(null, "NOT", null, null);       //11101000111000000000
        Asserts.assertFalse(acceptsFolder());             //  ++
        setFolderPatterns(null, null, null, null);        //11110000000010001000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns(null, "NOT", null, null);       //11101000111000000000
        Asserts.assertFalse(acceptsFolder());             //    ++
        setFolderPatterns("folder", "NOT", null, null);   //10001111000010001000
        Asserts.assertTrue(acceptsFolder());
        // the first and the third term are correlated:
        // (whiteFind == null) = ! (whiteFind != null)
        // -> we can not create a test case to cover the third term 

        setFolderPatterns("NOT", "NOT", null, null);      //10001110111000000000
        Asserts.assertFalse(acceptsFolder());             //      ++
        setFolderPatterns("folder", "NOT", null, null);   //10001111000010001000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns("NOT", null, null, null);       //10001110100000000000
        Asserts.assertFalse(acceptsFolder());             //        ++
        setFolderPatterns("NOT", "folder 1", null, null); //10001110111110001000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns("NOT", "NOT", null, null);      //10001110111000000000
        Asserts.assertFalse(acceptsFolder());             //          ++
        setFolderPatterns("NOT", "folder 1", null, null); //10001110111110001000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns(null, null, "1", null);         //11110000000011110000
        Asserts.assertFalse(acceptsFolder());             //            ++
        setFolderPatterns(null, null, null, null);        //11110000000010001000

        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns(null, null, "1", null);         //11110000000011110000
        Asserts.assertFalse(acceptsFolder());             //              ++
        setFolderPatterns(null, null, "2", null);         //11110000000011101000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns(null, null, null, "folder 1");  //11110000000010001111
        Asserts.assertFalse(acceptsFolder());             //                ++
        setFolderPatterns(null, null, null, null);        //11110000000010001000
        Asserts.assertTrue(acceptsFolder());

        setFolderPatterns(null, null, null, "folder 1");  //11110000000010001111
        Asserts.assertFalse(acceptsFolder());             //                  ++
        setFolderPatterns(null, null, null, "folder 2");  //11110000000010001110
        Asserts.assertTrue(acceptsFolder());
    }
}

class Asserts {
    public static void assertTrue(boolean b) {
        if (!b) {
            fail("true expected, but was false", 1);
        }
    }

    public static void assertFalse(boolean b) {
        if (b) {
            fail("false expected, but was true", 1);
        }
    }

    public static void fail(String message, int levelsToSkip) {
        System.out.println("FAILED " + Thread.currentThread().getStackTrace()[levelsToSkip + 3] + 
                "\n" + message);
    }

    public static void fail(String message) {
        fail(message, 1);
    }

    public static void fail() {
        fail("", 1);
    }
}