///////////////////////////////////////////////////////////////////////////////
//
// $Id: InstrumentationJob.java 1 2007-12-12 17:37:26Z t-scheller $
// 
// created at: 30.03.2007 15:22:14
//
///////////////////////////////////////////////////////////////////////////////

package org.gbt2.instrumentation;

import java.io.File;

/**
 * Is a container for an instrumentation job.<br>
 * <br>
 * It contains the source file to be instrumented and the target for the
 * instrumentation process of this file.
 * 
 * @author Christoph Müller
 * @version 1.0 - 25.02.2007
 */
public class InstrumentationJob {
    private File source;

    private File target;

    /**
     * A new {@link InstrumentJob}.
     * 
     * @param source
     *            The source file.
     * @param target
     *            The target file.
     */
    InstrumentationJob(File source, File target) {
        this.source = source;
        this.target = target;
    }

    /**
     * @return The source file.
     */
    public File getSource() {
        return this.source;
    }

    /**
     * @return The target file.
     */
    public File getTarget() {
        return this.target;
    }
}