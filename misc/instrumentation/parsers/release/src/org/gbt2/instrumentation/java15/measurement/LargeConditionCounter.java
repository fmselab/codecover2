///////////////////////////////////////////////////////////////////////////////
//
// $Id: LargeConditionCounter.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created by: Christoph Müller
// created at: 22.03.2007 08:48:50
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation.java15.measurement;

import java.util.HashMap;
import java.util.Map.Entry;

/**
 * This is a special {@link CounterContainer}, which is used for boolean
 * assignments.<br>
 * <br>
 * These assignments are coded using an integer bit mask. Then a mapping from a
 * bit mask to a counter is saved using a {@link HashMap}.
 * 
 * @author Christoph Müller
 * 
 * TODO Implement a container for small bit masks.
 * TODO Implement a container for multiple bit masks.
 * TODO threadsafe
 * TODO comments
 */
public class LargeConditionCounter extends CounterContainer {

    private HashMap<Integer, LongContainer> counters;

    private String varname;

    /**
     * Constructor for a new {@link LargeConditionCounter}.<br>
     * <br>
     * The HashMap is initialized.
     * 
     * @param fullClassName
     *            The full name of the instrumented class, this condition
     *            counter is placed in.
     * 
     * @param varname
     *            The name of the counter ID, that is passed to the
     *            {@link CoverageCounterLog}.
     */
    public LargeConditionCounter(String fullClassName, String varname) {
        super(fullClassName + "." + varname);
        this.varname = varname;
        this.counters = new HashMap<Integer, LongContainer>(17);
    }

    /**
     * Add a bit mask.<br>
     * <br>
     * If this bit mask is not in the HashMap, it is added with counter 1. Else
     * the counter is incremented.
     * 
     * @param booleanAssignments
     *            The assigments of the the basic boolean terms.
     * 
     * @return Always true.
     */
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

    @Override
    public void reset() {
        this.counters.clear();
    }

    @Override
    public void serializeAndReset(CoverageCounterLog log) {
        for (Entry<Integer, LongContainer> thisEntry : this.counters.entrySet()) {
            Integer integerKey = thisEntry.getKey();
            LongContainer counter = thisEntry.getValue();
            log.passCounter(this.varname + "-"
                    + Integer.toBinaryString(integerKey.intValue()),
                    counter.getValue());
        }

        reset();
    }
}
