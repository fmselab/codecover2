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

package org.codecover.instrumentation.java.measurement;

import junit.framework.TestCase;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: MeasurementTimerTest.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class MeasurementTimerTest extends TestCase {

    /**
     * Test method for {@link org.codecover.instrumentation.java.measurement.MeasurementTimer#getPhaseDuration()}.
     */
    public void testGetPhaseDuration() {
        MeasurementTimer timer = new MeasurementTimer();
        try {
            Thread.sleep(1200);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        timer.setOverallEnd();
        System.out.println(timer.getOverallDuration());
        timer.setPhaseEnd();
        System.out.println(timer.getPhaseDuration());
    }
}
