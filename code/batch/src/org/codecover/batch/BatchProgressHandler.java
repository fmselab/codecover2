/******************************************************************************
 * Copyright (c) 2007 Stefan Franke, Robert Hanussek, Benjamin Keil,          *
 *                    Steffen Kieß, Johannes Langauf,                         *
 *                    Christoph Marian Müller, Igor Podolskiy,                *
 *                    Tilmann Scheller, Michael Starzmann, Markus Wittlinger  *
 * All rights reserved. This file may be used, modifies and redistributed     *
 * under the terms of either the Eclipse Public License v1.0 which            *
 * accompanies this distribution and is available at                          *
 * http://www.eclipse.org/legal/epl-v10.html or the MIT license, available at *
 * http://www.opensource.org/licenses/mit-license.php                         *
 ******************************************************************************/

package org.codecover.batch;

import org.codecover.model.utils.ChangeListener;
import org.codecover.model.utils.ChangeType;
import org.codecover.model.utils.Logger;
import org.codecover.model.utils.ProgressHandler;

/**
 * Common methods for a progress handler that can display a progress bar. In a
 * shell window. 
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: BatchProgressHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class BatchProgressHandler implements ProgressHandler {
    /**
     * Creates a {@link BatchProgressHandler} with the given parameters
     * 
     * @param cl
     *            the given {@link CommandLine}
     * @param logger
     *            the given {@link Logger}
     * @return the instance of {@link BatchProgressHandler}
     */
    public static BatchProgressHandler createBatchProgressHandler(
            CommandLine cl, BatchLogger logger) {
        if (cl.hasOption(Options.progressBar) && cl.hasOption(Options.noProgressBar)) {
            logger.fatal("You cant't combine --progress-bar and --no-progress-bar");
        }
        boolean showProgressBar = !cl.hasOption(Options.quiet);
        if (cl.hasOption(Options.progressBar)) {
            showProgressBar = true;
        } else if (cl.hasOption(Options.noProgressBar)) {
            showProgressBar = false;
        }
        if (showProgressBar) {
            final RealBatchProgressHandler progressHandler = new RealBatchProgressHandler();
            logger.addPreWriteListener(new ChangeListener<String>() {
                public void changed(ChangeType changeType, String str) {
                    progressHandler.clearLine();
                }
            });
            logger.addPostWriteListener(new ChangeListener<String>() {
                public void changed(ChangeType changeType, String str) {
                    progressHandler.showLine();
                }
            });
            return progressHandler;
        } else {
            return NoBatchProgressHandler.instance;
        }
    }

    /**
     * Finishes the progress.
     */
    public abstract void complete();
}

/**
 * A dummy progress handler that displays nothing.  
 * 
 * @author Steffen Kieß 
 *
 * @version 1.0 ($Id: BatchProgressHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
class NoBatchProgressHandler extends BatchProgressHandler {
    /**
     * The instance of {@link NoBatchProgressHandler}
     */
    static final NoBatchProgressHandler instance = new NoBatchProgressHandler();

    private NoBatchProgressHandler() {
        // Do nothing.
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.batch.BatchProgressHandler#complete()
     */
    @Override
    public void complete() {
        // Do nothing.
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.utils.ProgressHandler#setProgress(float)
     */
    public void setProgress(float progress) {
        // Do nothing.
    }
}

/**
 * A progress handler that displays a progress bar. In a  shell window. 
 * 
 * @author Steffen Kieß 
 *
 * @version 1.0 ($Id: BatchProgressHandler.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
class RealBatchProgressHandler extends BatchProgressHandler {

    private static final int NR_DOTS = 60;

    private final static String SPINNER_CHARS = "\\|/-";

    private boolean printedNewline = true;

    private boolean isHidden = false;

    private float lastValue = 0;

    private int spinnerPos = 0;

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.model.utils.ProgressHandler#setProgress(float)
     */
    public void setProgress(float progress) {
        if (progress < 0.0 || progress > 1.0) {
            throw new IllegalArgumentException("progress < 0.0 || progress > 1.0");
        }

        // More or less stolen from e2fsprogs (e2fsck/unix.c)

        if ((int) ((progress * 1000) + 0.5) == (int) ((this.lastValue * 1000) + 0.5)) {
            // No change
            if (!this.printedNewline) {
                return;
            }
        } else {
            this.lastValue = progress;
            this.spinnerPos = (this.spinnerPos + 1) & 3;
        }

        final int completedDots = Math.round(NR_DOTS * progress);
        final int uncompletedDots = NR_DOTS - completedDots;

        final StringBuilder sb = new StringBuilder();
        sb.append("|");
        for (int i = 0; i < completedDots; i++) {
            sb.append("=");
        }
        for (int i = 0; i < uncompletedDots; i++) {
            if (i == 0) {
                sb.append(SPINNER_CHARS.charAt(this.spinnerPos));
            } else {
                sb.append(" ");
            }
        }
        sb.append("| ");

        sb.append(String.format("[%5.1f%%]", progress * 100.0f));

        sb.append("\r");

        System.out.print(sb.toString());
        System.out.flush();
        this.printedNewline = false;
        this.isHidden = false;
    }

    /**
     * Clears the line
     */
    public void clearLine() {
        if (!this.printedNewline) {
            final StringBuilder sb = new StringBuilder();
            for (int i = 0; i < NR_DOTS + 11; i++) {
                sb.append(" ");
            }
            sb.append("\r");
            System.out.print(sb.toString());
            System.out.flush();
            this.printedNewline = true;
            this.isHidden = true;
        }
    }

    /**
     * Shows the last set value.
     */
    public void showLine() {
        if (this.printedNewline && this.isHidden) {
            setProgress(this.lastValue);
        }
    }

    /**
     * (non-Javadoc)
     * 
     * @see org.codecover.batch.BatchProgressHandler#complete()
     */
    @Override
    public void complete() {
        setProgress(1.0f);
        if (!this.printedNewline) {
            System.out.println();
            this.printedNewline = true;
            this.isHidden = false;
        }
    }
}
