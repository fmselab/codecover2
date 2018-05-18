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

import java.util.ArrayList;
import java.util.List;

import org.apache.tools.ant.Task;

/**
 *
 * @author Steffen Kieß
 * @version 1.0 ($Id: CodecoverTask.java 50 2009-06-01 09:59:34Z ahija $)
 *
 */
public class CodecoverTask extends Task {
    private List<Command> commands = new ArrayList<Command>();

    /**
     * Adds the {@link LoadCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredLoad(LoadCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link SaveCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredSave(SaveCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link CreateContainerCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredCreateContainer(CreateContainerCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link InstrumentCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredInstrument(InstrumentCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link InstrumentCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredInstrumentMulti(InstrumentMultiCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link AnalyzeCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredAnalyze(AnalyzeCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link AnalyzeMultiCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredAnalyzeMulti(AnalyzeMultiCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link ReportCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredReport(ReportCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link MergeSessionsCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredMergeSessions(MergeSessionsCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link AlterSessionCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredAlterSession(AlterSessionCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link CopySessionsCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredCopySessions(CopySessionsCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link RemoveSessionsCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredRemoveSessions(RemoveSessionsCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link MergeTestCasesCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredMergeTestCases(MergeTestCasesCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link AlterTestCaseCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredAlterTestCase(AlterTestCaseCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link RemoveTestCasesCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addConfiguredRemoveTestCases(RemoveTestCasesCommand command) {
        this.commands.add(command);
    }

    /**
     * Adds the {@link AddPluginDirCommand}.
     *
     * @param command
     *                the configured command.
     */
    public void addAddPluginDir(AddPluginDirCommand command) {
        this.commands.add(command);
    }

    @Override
    public void execute() {
        final Context context = new Context(getProject(), new AntLogger(
                getProject()));

        for (Command command : this.commands) {
            command.run(context);
        }
    }
}
