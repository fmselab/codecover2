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

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;

import org.codecover.instrumentation.measurement.CoverageResultLog;

/**
 * This is an object, that is used for time comments in the
 * {@link CoverageResultLog}.<br>
 * <br>
 * It contains four timestamps of the type {@link GregorianCalendar}. The first
 * two represent the start and the end of the whole instrumentation process. The
 * other two represent the start and the end of the last phase&mdash;e.g. the
 * start and the end of the last test case.
 * 
 * @author Christoph Müller
 * @version 1.0 ($Id: MeasurementTimer.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 * TODO Test
 */
public class MeasurementTimer {
    /**
     * The format for the timestamp formatting:<br>
     * <br>
     * dd.MM.yyyy HH:mm:ss.SSS
     * 
     * @see SimpleDateFormat
     */
    public static final DateFormat DATE_FORMAT = new SimpleDateFormat(
            "dd.MM.yyyy HH:mm:ss.SSS");

    private GregorianCalendar overallStart;

    private GregorianCalendar overallEnd;

    private GregorianCalendar lastPhaseStart;

    private GregorianCalendar lastPhaseEnd;

    /**
     * Constructs a new {@link MeasurementTimer} and sets the internal
     * timestamps to the current time.
     * 
     * @see GregorianCalendar#GregorianCalendar()
     */
    public MeasurementTimer() {
        this.overallStart = new GregorianCalendar();
        this.overallEnd = null;
        this.lastPhaseStart = new GregorianCalendar();
        this.lastPhaseEnd = null;
    }

    /**
     * Returns the overall start timestamp as a formatted String.
     * 
     * @return The overall start timestamp.
     * @see #DATE_FORMAT
     */
    public String getOverallStart() {
        return DATE_FORMAT.format(this.overallStart.getTime());
    }

    /**
     * Returns the overall start timestamp as a long.
     * 
     * @return The overall start timestamp.
     */
    public long getOverallStartLong() {
        return this.overallStart.getTimeInMillis();
    }

    /**
     * Sets the overall end.
     */
    public void setOverallEnd() {
        this.overallEnd = new GregorianCalendar();
    }

    /**
     * Returns the overall end timestamp as a formatted String.<br>
     * <br>
     * {@link #setOverallEnd()} must have been called first.
     * 
     * @return The overall end timestamp.
     * @see #DATE_FORMAT
     */
    public String getOverallEnd() {
        return DATE_FORMAT.format(this.overallEnd.getTime());
    }

    /**
     * Returns the overall end timestamp as a long.<br>
     * <br>
     * {@link #setOverallEnd()} must have been called first.
     * 
     * @return The overall end timestamp.
     */
    public long getOverallEndLong() {
        return this.overallEnd.getTimeInMillis();
    }

    /**
     * Resets the last phase start timestamp.
     */
    public void setPhaseStart() {
        this.lastPhaseStart = new GregorianCalendar();
        this.lastPhaseEnd = null;
    }

    /**
     * Returns the last phase start timestamp as a formatted String.<br>
     * <br>
     * {@link #setPhaseStart()} must have been called first.
     * 
     * @return The reseted start phase timestamp.
     * @see #DATE_FORMAT
     */
    public String getPhaseStart() {
        return DATE_FORMAT.format(this.lastPhaseStart.getTime());
    }

    /**
     * Returns the last phase start timestamp as a long.<br>
     * <br>
     * {@link #setPhaseStart()} must have been called first.
     * 
     * @return The reseted start phase timestamp.
     */
    public long getPhaseStartLong() {
        return this.lastPhaseStart.getTimeInMillis();
    }

    /**
     * Resets the last phase end timestamp.
     */
    public void setPhaseEnd() {
        this.lastPhaseEnd = new GregorianCalendar();
    }

    /**
     * Returns the last phase end timestamp as a formatted String.<br>
     * <br>
     * {@link #getPhaseEnd()} must have been called first.
     * 
     * @return The last phase end timestamp.
     * @see #DATE_FORMAT
     */
    public String getPhaseEnd() {
        return DATE_FORMAT.format(this.lastPhaseEnd.getTime());
    }

    /**
     * Returns the last phase end timestamp as a long.<br>
     * <br>
     * {@link #getPhaseEnd()} must have been called first.
     * 
     * @return The last phase end timestamp.
     */
    public long getPhaseEndLong() {
        return this.lastPhaseEnd.getTimeInMillis();
    }

    /**
     * Returns the duration of the overall.<br>
     * <br>
     * The {@link #setOverallEnd()} must have been called before.
     * 
     * @return The overall duration
     */
    public String getOverallDuration() {
        return getDurationAsString(this.overallEnd.getTimeInMillis()
                - this.overallStart.getTimeInMillis());
    }

    /**
     * Returns the duration of the last phase.<br>
     * <br>
     * The {@link #setPhaseStart()} and {@link #setPhaseEnd()} must have been
     * called before in this sequence.
     * 
     * @return The overall duration
     */
    public String getPhaseDuration() {
        return getDurationAsString(this.lastPhaseEnd.getTimeInMillis()
                - this.lastPhaseStart.getTimeInMillis());
    }

    /**
     * Creates a formated Sting out of a duration in milliseconds.<br>
     * <br>
     * The output has the style <code>hh:mm:ss.SSS</code>. If hours or hours
     * and minutes are zero, these values are not put out.
     * 
     * @param duration
     *            The duration in milliseconds&mdash;not larger than
     *            {@value Integer#MAX_VALUE}.
     * 
     * @return The formatted duration.
     */
    public static String getDurationAsString(long duration) {
        int durationInSeconds = (int)duration / 1000;
        int durationInMinutes = durationInSeconds / 60;
        int durationInHours = durationInMinutes / 60;
        int leftoverMillis = (int)duration % 1000;
        int leftOverSeconds = durationInSeconds % 60;
        int leftOverMinutes = durationInMinutes % 60;

        StringBuffer buffer = new StringBuffer();
        if (durationInHours > 0) {
            if (durationInHours < 10) {
                buffer.append("0");
            }
            buffer.append(durationInHours);
            buffer.append(':');
        }

        if (leftOverMinutes < 10) {
            buffer.append("0");
        }
        buffer.append(leftOverMinutes);
        buffer.append(':');

        if (leftOverSeconds < 10) {
            buffer.append("0");
        }
        buffer.append(leftOverSeconds);
        buffer.append('.');

        if (leftoverMillis < 100) {
            if (leftoverMillis < 10) {
                buffer.append("0");
            }
            buffer.append("0");
        }
        buffer.append(leftoverMillis);

        return buffer.toString();
    }
}
