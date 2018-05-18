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
import org.codecover.model.TestSession;
import org.codecover.model.TestSessionContainer;
import org.codecover.model.exceptions.NameAlreadyUsedException;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: AlterSessionCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AlterSessionCommand extends Command {
    String containerId;

    String session;

    String name;

    String comment;

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
     * Sets the session.
     * 
     * @param session
     *                the session to set
     */
    public void setSession(String session) {
        this.session = session;
    }

    /**
     * Sets the name.
     * 
     * @param name
     *                the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Sets the comment.
     * 
     * @param comment
     *                the comment to set
     */
    public void setComment(String comment) {
        this.comment = comment;
    }

    @Override
    public void run(Context context) {
        if (this.containerId == null) {
            throw new BuildException("The attribute 'containerId' is missing.");
        }

        if (this.session == null) {
            throw new BuildException("The attribute 'session' is missing.");
        }

        TestSessionContainer testSessionContainer = context
                .getTestSessionContainer(this.containerId);

        TestSession testSession = testSessionContainer
                .getTestSessionWithName(this.session);

        if (testSession == null) {
            context.getLogger().fatal(
                    "Session '" + this.session + "' not found.");
        }

        if (this.name != null) {
            String newTestSessionName = this.name;
            try {
                testSession.setName(newTestSessionName);
                context.getLogger().info(
                        "Changed name of test session from \"" + this.session
                                + "\" to \"" + newTestSessionName + "\".");
            } catch (NameAlreadyUsedException e) {
                context.getLogger().warning("Name could not be set", e);
            }
        }

        if (this.comment != null) {
            String oldTestSessionComment = testSession.getComment();
            String newTestSessionComment = this.comment;
            testSession.setComment(newTestSessionComment);
            context.getLogger().info(
                    "Changed comment of test session from \""
                            + oldTestSessionComment + "\" to \""
                            + newTestSessionComment + "\".");
        }
    }
}
