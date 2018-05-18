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

package org.codecover.report.html;

import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

/**
 * This class provides an easy attempt for the sorting of lists mainly within
 * a velocity template. A key can be set to each Object (of the list), and when
 * called sort(List), the items of that list are sorted according to the key.
 * <br>
 * That key can be either an integer, a double or a string, the key which was
 * set last is the one used. If Strings and numbers are compared, the numbers
 * come first.   
 * 
 * @author Michael Starzmann
 * @version 1.0 ($Id: Sorter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class Sorter {
    
    private HashMap<Object, Double> numberKeys = new HashMap<Object, Double>();
    private HashMap<Object, String> stringKeys = new HashMap<Object, String>();
    
    
    Comparator comp = new Comparator() {
        /**
         * Compares two Objects using the corresponding keys saved either in
         * numberKeys or in stringKeys. If no key is saved at all, 0 is
         * returned. Otherwise, numbers are sorted in ascending order, Strings
         * lexicographically and behind the numbers. 
         * 
         * @param o1 first Object
         * @param o2 second Object
         * @return int smaller, equal, larger than zero,
         *             if o1 is smaller, equal or larger than o2
         */
        public int compare(Object o1, Object o2) {
            Double na = numberKeys.get(o1);
            Double nb = numberKeys.get(o2);
            String sa = stringKeys.get(o1);
            String sb = stringKeys.get(o2);
            if (na == null && sa == null && nb == null && sb == null) {
                return 0; // no key at all -> a == b
            } else if (nb == null && sb == null) {
                return -1; // a has key, b hasn't -> a < b 
            } else if (na == null && sa == null) {
                return 1; // b has key, a hasn't  -> a > b
            } else if (na == null && nb != null) {
                return 1; // b with numberKey before a with stringKey -> a > b
            } else if (na != null && nb == null) {
                return -1; // a with numberKey before b with stringKey -> a < b
            } else if (na != null && nb != null) {
                return Double.compare(na, nb);
            } else {
                return sa.compareTo(sb);
            }
        }
    };

    /**
     * Sets a key for object, according to which it can be sorted using the
     * function {@link #sort(List list)}. This overwrites any previously set key
     * for object.
     * 
     * @param object Object that should get 
     * @param key as sortingkey
     */
    public void setKey(Object object, double key) {
        stringKeys.remove(object);
        Double d = numberKeys.get(object);
        if (d == null) {
            numberKeys.put(object, new Double(key));
        } else {
            d = new Double (key);
        }
    }

    /**
     * Sets a key for object, according to which it can be sorted using the
     * function {@link #sort(List list)}. This overwrites any previously set key
     * for object.
     * 
     * @param object Object that should get 
     * @param key as sortingkey
     */
    public void setKey(Object object, int key) {
        stringKeys.remove(object);
        Double d = numberKeys.get(object);
        if (d == null) {
            numberKeys.put(object, new Double(key));
        } else {
            d = new Double (key);
        }
    }
    
    /**
     * Sets a key for object, according to which it can be sorted using the
     * function {@link #sort(List list)}. This overwrites any previously set key
     * for object.
     * 
     * @param object Object that should get 
     * @param key as sortingkey
     */    
    public void setKey(Object object, String key) {
        numberKeys.remove(object);
        String s = stringKeys.get(object);
        if (s == null) {
            stringKeys.put(object, key);
        } else {
            s = key;
        }
    }
    
    /**
     * Sorts the list according to previously set keys (see {@link #setKey})
     * for each item. Items without keys sorted to the end of the list, items
     * with numerical keys are sorted at the beginning, ascending, followed by
     * items with Strings as keys, sorted lexicographically.
     * 
     * @param list the list that should be sorted
     */
    @SuppressWarnings("unchecked")
    public void sort(List list) {
        Collections.sort(list, comp);
    }
}
