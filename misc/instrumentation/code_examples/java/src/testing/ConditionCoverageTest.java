///////////////////////////////////////////////////////////////////////////////
//
// $Id: ConditionCoverageTest.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 17:40:42
//
///////////////////////////////////////////////////////////////////////////////

package testing;

import org.gbt2.measurement.AlternativeCounterContainer;
import org.gbt2.measurement.ConditionCounter;
import org.gbt2.measurement.CounterContainer;
import org.gbt2.measurement.LongContainer;
import org.gbt2.measurement.MeasurementHelper;
import org.gbt2.measurement.SynchronizedConditionCounter;

/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 */
public class ConditionCoverageTest {
    public static final long MAX_COUNT = Long.rotateLeft(1L, 25);
    
    public static final boolean first = true;
    public static final boolean second = true;
    public static final boolean third = true;
    
    public static void main(String[] args) {
        long timer;

        timer = System.currentTimeMillis();
        without();
        timer = System.currentTimeMillis() - timer;
        printResult("without", timer);
        
        timer = System.currentTimeMillis();
        withSmallboolean();
        timer = System.currentTimeMillis() - timer;
        printResult("withSmallboolean", timer);
        
        timer = System.currentTimeMillis();
        withBytes();
        timer = System.currentTimeMillis() - timer;
        printResult("withBytes", timer);
        
        timer = System.currentTimeMillis();
        withIntegers();
        timer = System.currentTimeMillis() - timer;
        printResult("withIntegers", timer);

        timer = System.currentTimeMillis();
        withOptimizedIntegers();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedIntegers", timer);      
        
        timer = System.currentTimeMillis();
        withOptimizedIntegers2();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedIntegers2", timer);      
        
        timer = System.currentTimeMillis();
        withOptimizedIntegersAndHash();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedIntegersAndHash", timer);      
        
        timer = System.currentTimeMillis();
        withOptimizedIntegersAndSynchronizedHash();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedIntegersAndSynchronizedHash", timer);      
        
        timer = System.currentTimeMillis();
        withLongs();
        timer = System.currentTimeMillis() - timer;
        printResult("withLongs", timer);
        
        timer = System.currentTimeMillis();
        withOptimizedLongs();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedLongs", timer);  
        
        timer = System.currentTimeMillis();
        withAlternativeCounterContainer();
        timer = System.currentTimeMillis() - timer;
        printResult("withAlternativeCounterContainer", timer);  
        
        timer = System.currentTimeMillis();
        withOptimizedAlternativeCounterContainer();
        timer = System.currentTimeMillis() - timer;
        printResult("withOptimizedAlternativeCounterContainer", timer);  
        
        timer = System.currentTimeMillis();
        withFinishAlternativeCounterContainer();
        timer = System.currentTimeMillis() - timer;
        printResult("withFinishAlternativeCounterContainer", timer);  
        
        timer = System.currentTimeMillis();
        withLongContainer();
        timer = System.currentTimeMillis() - timer;
        printResult("withLongContainer", timer);
        
        timer = System.currentTimeMillis();
        withBigBoolean();
        timer = System.currentTimeMillis() - timer;
        printResult("withBigBoolean", timer);
    }

    public static final void printResult(String method, long timer)
    {
        System.out.printf(method + "%n %14dms%n %14ds%n %14dmin%n%n",
                new Long(timer),
                new Long(timer / 1000L),
                new Long(timer / 1000L / 60L));
    }
    
    private static void without()
    {
        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            result = (first && second) || third;
        }
    }
    
    private static void withSmallboolean()
    {
        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            boolean used1 = false;
            boolean result1 = false;
            boolean used2 = false;
            boolean result2 = false;
            boolean used3 = false;
            boolean result3 = false;
            
            result = ((((used1 = true) & first) && (result1 = true))
            && (((used2 = true) & second) && (result2 = true)))
            || (((used3 = true) & third) && (result3 = true));
        }
    }
    
    private static void withBytes() {
        final byte BIT00 = Byte.parseByte("1", 2);
        final byte BIT01 = Byte.parseByte("10", 2);
        final byte BIT02 = Byte.parseByte("100", 2);
        final byte BIT03 = Byte.parseByte("1000", 2);
        final byte BIT04 = Byte.parseByte("10000", 2);
        final byte BIT05 = Byte.parseByte("100000", 2);

        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            byte terms0001 = (byte)0;
            
            result = ((((terms0001 |= BIT00) >= Byte.MIN_VALUE) & first) && ((terms0001 |= BIT01) >= Byte.MIN_VALUE))
            && ((((terms0001 |= BIT02) >= Byte.MIN_VALUE) & second) && ((terms0001 |= BIT03) >= Byte.MIN_VALUE))
            || ((((terms0001 |= BIT04) >= Byte.MIN_VALUE) & third) && ((terms0001 |= BIT05) >= Byte.MIN_VALUE));
        }
    }

    private static void withIntegers() {
        final int BIT00 = Integer.parseInt("1", 2);
        final int BIT01 = Integer.parseInt("10", 2);
        final int BIT02 = Integer.parseInt("100", 2);
        final int BIT03 = Integer.parseInt("1000", 2);
        final int BIT04 = Integer.parseInt("10000", 2);
        final int BIT05 = Integer.parseInt("100000", 2);

        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            int terms0001 = 0;

            result = ((((terms0001 |= BIT00) >= Integer.MIN_VALUE) & first) && ((terms0001 |= BIT01) >= Integer.MIN_VALUE))
            && ((((terms0001 |= BIT02) >= Integer.MIN_VALUE) & second) && ((terms0001 |= BIT03) >= Integer.MIN_VALUE))
            || ((((terms0001 |= BIT04) >= Integer.MIN_VALUE) & third) && ((terms0001 |= BIT05) >= Integer.MIN_VALUE));
        }
    }

    private static void withOptimizedIntegers() {
        final int BIT00 = Integer.parseInt("1", 2);
        final int BIT01 = Integer.parseInt("10", 2);
        final int BIT02 = Integer.parseInt("100", 2);
        final int BIT03 = Integer.parseInt("1000", 2);
        final int BIT04 = Integer.parseInt("10000", 2);
        final int BIT05 = Integer.parseInt("100000", 2);

        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            int terms0001 = 0;

            result = ((((terms0001 |= BIT05) == 0 | true) & first) && ((terms0001 |= BIT04) == 0 | true))
            && ((((terms0001 |= BIT03) == 0 | true) & second) && ((terms0001 |= BIT02) == 0 | true))
            || ((((terms0001 |= BIT01) == 0 | true) & third) && ((terms0001 |= BIT00) == 0 | true));
        }
    }
    
    private static void withOptimizedIntegers2() {
        final int BIT00 = Integer.parseInt("1", 2);
        final int BIT01 = Integer.parseInt("10", 2);
        final int BIT02 = Integer.parseInt("100", 2);
        final int BIT03 = Integer.parseInt("1000", 2);
        final int BIT04 = Integer.parseInt("10000", 2);
        final int BIT05 = Integer.parseInt("100000", 2);

        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            int terms0001 = 0;

            result = ((((terms0001 |= BIT05) != 0 | true) & first) && ((terms0001 |= BIT04) != 0 | true))
            && ((((terms0001 |= BIT03) != 0 | true) & second) && ((terms0001 |= BIT02) != 0 | true))
            || ((((terms0001 |= BIT01) != 0 | true) & third) && ((terms0001 |= BIT00) != 0 | true));
        }
    }
    
    private static void withOptimizedIntegersAndHash() {
        final int BIT00 = Integer.parseInt("1", 2);
        final int BIT01 = Integer.parseInt("10", 2);
        final int BIT02 = Integer.parseInt("100", 2);
        final int BIT03 = Integer.parseInt("1000", 2);
        final int BIT04 = Integer.parseInt("10000", 2);
        final int BIT05 = Integer.parseInt("100000", 2);
        
        boolean result;
        int terms0001;
        ConditionCounter container = new ConditionCounter("withOptimizedIntegersAndHash");
        for (long i = 1; i <= MAX_COUNT; i++) {
            result = ((terms0001 = 0) == 0) & (((((terms0001 |= BIT05) == 0 | true) & first) && ((terms0001 |= BIT04) == 0 | true))
            && ((((terms0001 |= BIT03) == 0 | true) & second) && ((terms0001 |= BIT02) == 0 | true))
            || ((((terms0001 |= BIT01) == 0 | true) & third) && ((terms0001 |= BIT00) == 0 | true))) & container.add(terms0001);
        }
    }
    
    private static void withOptimizedIntegersAndSynchronizedHash() {
        final int BIT00 = Integer.parseInt("1", 2);
        final int BIT01 = Integer.parseInt("10", 2);
        final int BIT02 = Integer.parseInt("100", 2);
        final int BIT03 = Integer.parseInt("1000", 2);
        final int BIT04 = Integer.parseInt("10000", 2);
        final int BIT05 = Integer.parseInt("100000", 2);
        
        boolean result;
        int terms0001;
        SynchronizedConditionCounter container = new SynchronizedConditionCounter("withOptimizedIntegersAndHash");
        for (long i = 1; i <= MAX_COUNT; i++) {
            result = ((terms0001 = 0) == 0) & (((((terms0001 |= BIT05) == 0 | true) & first) && ((terms0001 |= BIT04) == 0 | true))
                    && ((((terms0001 |= BIT03) == 0 | true) & second) && ((terms0001 |= BIT02) == 0 | true))
                    || ((((terms0001 |= BIT01) == 0 | true) & third) && ((terms0001 |= BIT00) == 0 | true))) & container.add(terms0001);
        }
    }

    private static void withLongs()
    {
        final long BIT00 = Long.parseLong("1", 2);
        final long BIT01 = Long.parseLong("10", 2);
        final long BIT02 = Long.parseLong("100", 2);
        final long BIT03 = Long.parseLong("1000", 2);
        final long BIT04 = Long.parseLong("10000", 2);
        final long BIT05 = Long.parseLong("100000", 2);

        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            long terms0001 = 0L;
            
            result = ((((terms0001 |= BIT00) >= Long.MIN_VALUE) & first) && ((terms0001 |= BIT01) >= Long.MIN_VALUE))
            && ((((terms0001 |= BIT02) >= Long.MIN_VALUE) & second) && ((terms0001 |= BIT03) >= Long.MIN_VALUE))
            || ((((terms0001 |= BIT04) >= Long.MIN_VALUE) & third) && ((terms0001 |= BIT05) >= Long.MIN_VALUE));
        }
    }

    private static void withOptimizedLongs()
    {
        final long BIT00 = Long.parseLong("1", 2);
        final long BIT01 = Long.parseLong("10", 2);
        final long BIT02 = Long.parseLong("100", 2);
        final long BIT03 = Long.parseLong("1000", 2);
        final long BIT04 = Long.parseLong("10000", 2);
        final long BIT05 = Long.parseLong("100000", 2);
        
        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            long terms0001 = 0L;
            
            result = ((((terms0001 |= BIT00) == 0L | true) & first) && ((terms0001 |= BIT01) == 0L | true))
                     && ((((terms0001 |= BIT02) == 0L | true) & second) && ((terms0001 |= BIT03) == 0L | true))
                     || ((((terms0001 |= BIT04) == 0L | true) & third) && ((terms0001 |= BIT05) == 0L | true));
        }
    }
    
    private static void withAlternativeCounterContainer()
    {
        boolean result;
        AlternativeCounterContainer altCounterContainer = new AlternativeCounterContainer();

        for (long i = 1; i <= MAX_COUNT; i++) {
            result = altCounterContainer.reset() &
                    ((((altCounterContainer.setUsedBit(1)) & first) && (altCounterContainer.setTrueBit(1)))
                    && (((altCounterContainer.setUsedBit(2)) & second) && (altCounterContainer.setTrueBit(2)))
                    || (((altCounterContainer.setUsedBit(3)) & third) && (altCounterContainer.setTrueBit(3))));
        }
    }
    
    private static void withOptimizedAlternativeCounterContainer()
    {
        boolean result;
        AlternativeCounterContainer altCounterContainer = new AlternativeCounterContainer();

        for (long i = 1; i <= MAX_COUNT; i++) {
            result = altCounterContainer.reset() &
                    ((((altCounterContainer.setUsedBit(1) | true) & first) && (altCounterContainer.setTrueBit(1) | true))
                    && (((altCounterContainer.setUsedBit(2) | true) & second) && (altCounterContainer.setTrueBit(2) | true))
                    || (((altCounterContainer.setUsedBit(3) | true) & third) && (altCounterContainer.setTrueBit(3) | true)));
        }
    }
    
    private static void withFinishAlternativeCounterContainer()
    {
        boolean result;
        AlternativeCounterContainer altCounterContainer = new AlternativeCounterContainer();
        
        for (long i = 1; i <= MAX_COUNT; i++) {
            
            result = altCounterContainer.reset() &
            ((((altCounterContainer.setUsedBit(1) | true) & first) && (altCounterContainer.setTrueBit(1) | true))
                    && (((altCounterContainer.setUsedBit(2) | true) & second) && (altCounterContainer.setTrueBit(2) | true))
                    || (((altCounterContainer.setUsedBit(3) | true) & third) && (altCounterContainer.setTrueBit(3) | true)))
                    & altCounterContainer.finish();
        }
    }

    private static void withLongContainer()
    {
        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            LongContainer data = new LongContainer();
            
            result = (MeasurementHelper.evaluate(data, 3, first)
                    && MeasurementHelper.evaluate(data, 2, second))
                    || MeasurementHelper.evaluate(data, 1, third);
        }
    }
    
    private static void withBigBoolean()
    {
        boolean result;
        for (long i = 1; i <= MAX_COUNT; i++) {
            Boolean res1 = null;
            Boolean res2 = null;
            Boolean res3 = null;
            result = ((res1 = new Boolean(first)).booleanValue()
                    && (res2 = new Boolean(second)).booleanValue())
                    || (res3 = new Boolean(third)).booleanValue();
        }
    }
}
