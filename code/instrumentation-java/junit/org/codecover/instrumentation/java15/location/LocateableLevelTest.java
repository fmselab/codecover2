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
package org.codecover.instrumentation.java15.location;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.codecover.UtilsForTestingJava;
import org.codecover.model.MASTBuilder;
import org.codecover.model.mast.Location;
import org.codecover.model.mast.SourceFile;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: LocateableLevelTest.java 15 2008-05-24 20:59:06Z ahija $)
 */
public class LocateableLevelTest extends TestCase {
    private static final MASTBuilder BUILDER = UtilsForTestingJava.newMASTBuilder(); 
    private static final SourceFile SOURCE = BUILDER.createSourceFile("name", Long.toString(Long.MAX_VALUE, 2));

    public void test101() {
        for (int i = 0; i <= SOURCE.getContent().length() -1; i++) {
            for (int j = i + 1; j <= SOURCE.getContent().length(); j++) {
                List<Location> ll = getLocationList(i, j);
                Assert.assertEquals(1, ll.size());
                Assert.assertEquals(i, ll.get(0).getStartOffset());
                Assert.assertEquals(j, ll.get(0).getEndOffset());
            }
        }
    }

    public void test201() {
        try {
            getLocationList(-1, 10);
            Assert.fail();
        } catch (IllegalArgumentException e) {
            // expected
        }
    }

    public void test301() {
        List<Location> ll = getLocationList(10, 40, 0, 50);
        Assert.assertEquals(0, ll.size());
    }
    
    public void test302() {
        List<Location> ll = getLocationList(10, 40, 10, 40);
        Assert.assertEquals(0, ll.size());
    }
    
    public void test303() {
        List<Location> ll = getLocationList(10, 40, 0, 40);
        Assert.assertEquals(0, ll.size());
    }
    
    public void test304() {
        List<Location> ll = getLocationList(10, 40, 10, 50);
        Assert.assertEquals(0, ll.size());
    }
    
    public void test305() {
        List<Location> ll = getLocationList(10, 40, 0, 50);
        Assert.assertEquals(0, ll.size());
    }
    
    public void test306() {
        List<Location> ll = getLocationList(10, 40, 0, 10, 0, 9, 40, 50, 41, 51);
        Assert.assertEquals(1, ll.size());
        Assert.assertEquals(10, ll.get(0).getStartOffset());
        Assert.assertEquals(40, ll.get(0).getEndOffset());
    }

    public void test307() {
        List<Location> ll = getLocationList(10, 40, 0, 30);
        Assert.assertEquals(1, ll.size());
        Assert.assertEquals(30, ll.get(0).getStartOffset());
        Assert.assertEquals(40, ll.get(0).getEndOffset());
    }
    
    public void test308() {
        List<Location> ll = getLocationList(10, 40, 10, 30);
        Assert.assertEquals(1, ll.size());
        Assert.assertEquals(30, ll.get(0).getStartOffset());
        Assert.assertEquals(40, ll.get(0).getEndOffset());
    }

    public void test309() {
        List<Location> ll = getLocationList(10, 40, 20, 50);
        Assert.assertEquals(1, ll.size());
        Assert.assertEquals(10, ll.get(0).getStartOffset());
        Assert.assertEquals(20, ll.get(0).getEndOffset());
    }
    
    public void test310() {
        List<Location> ll = getLocationList(10, 40, 20, 40);
        Assert.assertEquals(1, ll.size());
        Assert.assertEquals(10, ll.get(0).getStartOffset());
        Assert.assertEquals(20, ll.get(0).getEndOffset());
    }

    public void test311() {
        List<Location> ll = getLocationList(10, 40, 20, 30);
        Assert.assertEquals(2, ll.size());
        Assert.assertEquals(10, ll.get(0).getStartOffset());
        Assert.assertEquals(20, ll.get(0).getEndOffset());
        Assert.assertEquals(30, ll.get(1).getStartOffset());
        Assert.assertEquals(40, ll.get(1).getEndOffset());
    }

    public void test312() {
        List<Location> ll = getLocationList(10, 40, 5, 15, 20, 25, 30, 35, 39, 50);
        Assert.assertEquals(3, ll.size());
        Assert.assertEquals(15, ll.get(0).getStartOffset());
        Assert.assertEquals(20, ll.get(0).getEndOffset());
        Assert.assertEquals(25, ll.get(1).getStartOffset());
        Assert.assertEquals(30, ll.get(1).getEndOffset());
        Assert.assertEquals(35, ll.get(2).getStartOffset());
        Assert.assertEquals(39, ll.get(2).getEndOffset());
    }

    private List<Location> getLocationList(int start, int end, int ... slices) {
        LocateableLevel level = new LocateableLevel(false); 
        level.startOffset(start);
        Assert.assertEquals(0, slices.length %2);
        List<Location> listLocations = new ArrayList<Location>(slices.length / 2);
        for (int i = 0; i < slices.length; i += 2) {
            listLocations.add(BUILDER.createLocation(SOURCE, slices[i], slices[i + 1]));
        }
        level.sliceOut(BUILDER.createLocationList(listLocations));
        return level.toLocationList(end, SOURCE, BUILDER).getLocations();
    }
}
