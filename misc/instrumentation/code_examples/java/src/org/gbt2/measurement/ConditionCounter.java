///////////////////////////////////////////////////////////////////////////////
//
// $Id: ConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 22.03.2007 08:48:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.measurement;

import java.util.HashMap;
import java.util.Map.Entry;

/**
 * @author Christoph Müller
 * @version 1.0 - 22.03.2007
 */
public class ConditionCounter implements CounterContainer {
    public static final int BIT0 = 1;
    public static final int BIT1 = 2;
    public static final int BIT2 = 4;
    public static final int BIT3 = 8;
    public static final int BIT4 = 16;
    public static final int BIT5 = 32;
    public static final int BIT6 = 64;
    public static final int BIT7 = 128;
    public static final int BIT8 = 256;
    public static final int BIT9 = 512;
    public static final int BIT10 = 1024;
    public static final int BIT11 = 2048;
    public static final int BIT12 = 4096;
    public static final int BIT13 = 8192;
    public static final int BIT14 = 16384;
    public static final int BIT15 = 32768;
    public static final int BIT16 = 65536;
    public static final int BIT17 = 131072;
    public static final int BIT18 = 262144;
    public static final int BIT19 = 524288;
    public static final int BIT20 = 1048576;
    public static final int BIT21 = 2097152;
    public static final int BIT22 = 4194304;
    public static final int BIT23 = 8388608;
    public static final int BIT24 = 16777216;
    public static final int BIT25 = 33554432;
    public static final int BIT26 = 67108864;
    public static final int BIT27 = 134217728;
    public static final int BIT28 = 268435456;
    public static final int BIT29 = 536870912;
    public static final int BIT30 = 1073741824;
    public static final int BIT31 = -2147483648;

    private HashMap<Integer, LongContainer> counters;
    String varname;
    
    public ConditionCounter(String varname) {
        this.varname = varname;
        this.counters = new HashMap<Integer, LongContainer>(17);
    }

    public boolean add(int booleanAssignments) {
        Integer integerKey = new Integer(booleanAssignments);
        LongContainer counter = this.counters.get(integerKey);
        if (counter == null) {
            this.counters.put(integerKey, new LongContainer(1L));
        } else {
            counter.increment();
        }
        
        return true;
    }

    public void serializeAndReset(CoverageResultLog printer) {
        for (Entry<Integer, LongContainer> thisEntry : this.counters.entrySet()) {
            Integer integerKey = thisEntry.getKey();
            LongContainer counter = thisEntry.getValue();
            printer.writeCounter(this.varname + "-" + Long.toBinaryString(integerKey.intValue()),
                    counter.content);
        }
        
        this.counters.clear();
    }
}
