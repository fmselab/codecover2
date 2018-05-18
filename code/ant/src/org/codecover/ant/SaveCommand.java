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

package org.codecover.ant;

import java.io.File;

import org.apache.tools.ant.BuildException;
import org.codecover.model.exceptions.FileSaveException;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: SaveCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 * 
 */
public class SaveCommand extends Command {
    String containerId;

    File filename;

    boolean override = true;

    /**
     * Sets the containerId.
     * 
     * @param containerId
     *                the containerId to set
     */
    public void setContainerId(String containerId) {
        this.containerId = containerId;
    }

    /**
     * Sets the filename.
     * 
     * @param filename
     *                the filename to set
     */
    public void setFilename(File filename) {
        this.filename = filename;
    }

    /**
     * Sets the override.
     * 
     * @param override
     *                the override to set
     */
    public void setOverride(boolean override) {
        this.override = override;
    }

    @Override
    public void run(Context context) {
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        if (this.filename == null) {
            throw new BuildException("The attribute 'filename' is missing.");
        }

        context.getLogger().info("Saving " + this.filename);

        if (!this.override && this.filename.exists()) {
            // TODO: We have a race condition here. Solve this some other way?
            context.getLogger().fatal(
                    "Target file (" + this.filename + ") already exists.");
        }

        try {
            context.getTestSessionContainer(this.containerId).save(
                    this.filename);
        } catch (FileSaveException e) {
            context.getLogger().fatal(e.getMessage(), e);
        }

        context.getLogger().info("Successfully saved " + this.filename);
    }
}
