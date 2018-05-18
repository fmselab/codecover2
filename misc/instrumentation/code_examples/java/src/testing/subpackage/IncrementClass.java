///////////////////////////////////////////////////////////////////////////////
//
// $Id: IncrementClass.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 21.03.2007 10:30:46
//
///////////////////////////////////////////////////////////////////////////////

package testing.subpackage;

import java.util.Vector;

import testing.AwayIncrementClass;

/**
 * @author Christoph Müller
 * @version 1.0 - 21.03.2007
 */
public class IncrementClass {
    public static final long MAX_COUNT = Long.rotateLeft(1L, 28);

    public static long staticPublicCount = 0L;

    private static long staticPrivateCount = 0L;

    public long memberPublicCount = 0L;

    private long memberPrivateCount = 0L;

    private final Object LOCK_OBJECT = new Object();

    private static CounterEntry[] arrayCounters;

    private static long[] arrayLongs;

    private static Vector<CounterEntry> vectorCounters;

    static {
        arrayLongs = new long[10];
        arrayLongs[0] = 0L;

        arrayCounters = new CounterEntry[10];
        arrayCounters[0] = new CounterEntry("ID");

        vectorCounters = new Vector<CounterEntry>(10);
        vectorCounters.add(new CounterEntry("ID"));
    }

    public static void main(String[] args) {
        IncrementClass testClass = new IncrementClass();
        long timer;

        timer = System.currentTimeMillis();
        testClass.incrementPrivateMember();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPrivateMember", timer);

        timer = System.currentTimeMillis();
        testClass.incrementPublicMember();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPublicMember", timer);

        timer = System.currentTimeMillis();
        testClass.incrementPrivateStatic();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPrivateStatic", timer);

        timer = System.currentTimeMillis();
        testClass.incrementPublicStatic();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPublicStatic", timer);

        timer = System.currentTimeMillis();
        testClass.incrementPublicMemberAway();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPublicMemberAway", timer);

        timer = System.currentTimeMillis();
        testClass.incrementPublicStaticAway();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementPublicStaticAway", timer);
        
        timer = System.currentTimeMillis();
        testClass.incrementCounterEntryVector();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementCounterEntryVector", timer);

        timer = System.currentTimeMillis();
        testClass.incrementCounterEntryArray();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementCounterEntryArray", timer);
        
        timer = System.currentTimeMillis();
        testClass.incrementLongArray();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementLongArray", timer);
        
        timer = System.currentTimeMillis();
        testClass.incrementLongArraySynchronized();
        timer = System.currentTimeMillis() - timer;
        printResult("incrementLongArraySynchronized", timer);
    }

    public static final void printResult(String method, long timer)
    {
        System.out.printf(method + "%n %14dms%n %14ds%n %14dmin%n%n",
                new Long(timer),
                new Long(timer / 1000L),
                new Long(timer / 1000L / 60L));
    }
    
    private void incrementPrivateMember() {
        while (this.memberPrivateCount <= MAX_COUNT) {
            this.memberPrivateCount++;
        }
    }

    private void incrementPublicMember() {
        while (this.memberPublicCount <= MAX_COUNT) {
            this.memberPublicCount++;
        }
    }

    private void incrementPrivateStatic() {
        while (staticPrivateCount <= MAX_COUNT) {
            staticPrivateCount++;
        }
    }

    private void incrementPublicStatic() {
        while (staticPublicCount <= MAX_COUNT) {
            staticPublicCount++;
        }
    }

    private void incrementPublicStaticAway() {
        while (AwayIncrementClass.staticPublicCount <= MAX_COUNT) {
            AwayIncrementClass.staticPublicCount++;
        }
    }

    private void incrementPublicMemberAway() {
        AwayIncrementClass instance = new AwayIncrementClass();

        while (instance.memberPublicCount <= MAX_COUNT) {
            instance.memberPublicCount++;
        }
    }

    private void incrementCounterEntryVector() {
        while (vectorCounters.get(0).counter <= MAX_COUNT) {
            vectorCounters.get(0).counter++;
        }
    }

    private void incrementCounterEntryArray() {
        while (arrayCounters[0].counter <= MAX_COUNT) {
            arrayCounters[0].counter++;
        }
    }

    private void incrementLongArray() {
        arrayLongs[0] = 0;
        while (arrayLongs[0] <= MAX_COUNT) {
            arrayLongs[0]++;
        }
    }
    
    private void incrementLongArraySynchronized() {
        arrayLongs[0] = 0;
        while (arrayLongs[0] <= MAX_COUNT) {
            synchronized (this.LOCK_OBJECT ) {
                arrayLongs[0]++;                
            }
        }
    }

    public static final class CounterEntry
    {
        public String id;
        public long counter;

        /**
         * @param id
         * @param counter
         */
        public CounterEntry(String id) {
            this.id = id;
            this.counter = 0L;
        }
    }
}
