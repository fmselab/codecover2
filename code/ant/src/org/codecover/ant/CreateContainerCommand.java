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

import org.apache.tools.ant.BuildException;
import org.codecover.model.TestSessionContainer;

/**
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: CreateContainerCommand.java 1895 2007-09-17 11:16:31Z
 *          kiesssn $)
 * 
 */
public class CreateContainerCommand extends Command {
    String oldContainerId;

    String newContainerId;

    /**
     * Sets the oldContainerId.
     * 
     * @param oldContainerId
     *                the oldContainerId to set
     */
    public void setOldContainerId(String oldContainerId) {
        this.oldContainerId = oldContainerId;
    }

    /**
     * Sets the newContainerId.
     * 
     * @param newContainerId
     *                the newContainerId to set
     */
    public void setNewContainerId(String newContainerId) {
        this.newContainerId = newContainerId;
    }

    @Override
    public void run(Context context) {
        if (this.oldContainerId == null) {
            throw new BuildException(
                    "The attribute 'oldContainerId' is missing.");
        }

        if (this.newContainerId == null) {
            throw new BuildException(
                    "The attribute 'newContainerId' is missing.");
        }

        TestSessionContainer oldContainer = context
                .getTestSessionContainer(this.oldContainerId);

        context.setTestSessionContainer(this.newContainerId,
                new TestSessionContainer(oldContainer.getCode(), context
                        .getLogger(), oldContainer.getFiles(), oldContainer
                        .getCriteria(), oldContainer.getId(), oldContainer
                        .getDate()));
    }
}
