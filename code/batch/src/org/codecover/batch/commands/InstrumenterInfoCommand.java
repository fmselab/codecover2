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

package org.codecover.batch.commands;

import java.util.*;
import java.util.Map.Entry;

import org.codecover.batch.*;
import org.codecover.instrumentation.InstrumenterDescriptor;
import org.codecover.instrumentation.InstrumenterDirective;
import org.codecover.model.utils.criteria.Criterion;
import org.codecover.model.extensions.*;

/**
 * @author Christoph Müller
 *
 * @version 1.0 ($Id: InstrumenterInfoCommand.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class InstrumenterInfoCommand extends SimpleCommand {

    private static final String DESCRIPTION = "print information about " +
                                              "available instrumenters";

    private static final InstrumenterInfoCommand instance = new InstrumenterInfoCommand();

    /**
     * Gets an instance of this class
     * 
     * @return the instance of the class
     */
    public static InstrumenterInfoCommand getInstance() {
        return instance;
    }

    private InstrumenterInfoCommand() {
        super("ii",
              "instrumenter-info",
              DESCRIPTION,
              new OptionSet(
              // REQUIRED
              new Option[0],

              // NOT REQUIRED
              new Option[] {Options.language}));
    }

    @Override
    protected int run(CommandLine cl, BatchLogger logger, org.codecover.model.extensions.PluginManager pluginManager) {
        // instrumenter-info-command
        /*
         * you may want to use the attributes
         * - boolean isQuiet(cl)
         * - boolean isVerbose(cl)
         * (isQuiet(cl) AND isVerbose(cl) == false)
         * 
         * - boolean isPretend(cl)
         * - boolean isVersion(cl)
         */

        // ////////////////////////////////////////////////////////////////////
        //
        // get values from options
        //
        // ////////////////////////////////////////////////////////////////////
        // optional options
        final String pLanguage = cl.getOptionValueOrNull(Options.language);
        final boolean pHasLanguage = cl.hasOption(Options.language);

        Set<InstrumenterDescriptor> descriptors;
        if (pHasLanguage) {
            // print out just the instrumenters for this language
            descriptors = new HashSet<InstrumenterDescriptor>();
            for (final InstrumenterDescriptor descr : PluginUtils.getExtensionObjects(pluginManager, logger, InstrumenterDescriptor.class)) {
                if (descr.isLanguageSupported(pLanguage)) {
                    descriptors.add(descr);
                }
            }
        } else {
            // print out all instrumenters
            descriptors = PluginUtils.getExtensionObjects(pluginManager, logger, InstrumenterDescriptor.class);
        }

        if (descriptors.size() > 0) {
            Iterator<InstrumenterDescriptor> iterator = descriptors.iterator();
            StringBuilder output = new StringBuilder(descriptors.size() * 256);
            while (iterator.hasNext()) {
                InstrumenterDescriptor thisDescriptor = iterator.next();
                output.append(HelpFormatter.formatTable(new String[][]{
                            new String[]{"Unique key:", " ", thisDescriptor.getUniqueKey()},
                            new String[]{"Language:", " ", thisDescriptor.getLanguageName()},
                            new String[]{},
                        }));

                output.append(HelpFormatter.formatTable(new String[][]{
                            new String[]{"Description:"},
                        }));
                output.append(HelpFormatter.formatTable(new String[][]{
                            new String[]{" ", thisDescriptor.getDescription()},
                            new String[]{},
                        }));
    
                output.append(HelpFormatter.formatTable(new String[][]{
                            new String[]{"Supported criteria:"},
                        }));
                Set<Criterion> supportedCriteria = thisDescriptor.getSupportedCriteria();
                final List<List<String>> criteraTable = new ArrayList<List<String>>();
                for (Criterion thisCriterion : supportedCriteria) {
                    final List<String> row = new ArrayList<String>();
                    row.add(" ");
                    row.add(thisCriterion.getName());
                    // TODO: add stuff like shortcut, description?
                    criteraTable.add(row);
                }
                criteraTable.add(Collections.<String>emptyList());
                output.append(HelpFormatter.formatTable(criteraTable));

                if (isVerbose(cl)) {
                    output.append(HelpFormatter.formatTable(new String[][]{
                                new String[]{"Author:", " ", thisDescriptor.getAuthor()},
                                new String[]{"Default charset:", " ", thisDescriptor.getDefaultCharset().displayName()},
                                new String[]{"Class name:", " ", thisDescriptor.getClass().getName()},
                                new String[]{},
                            }));
                    
                    
                    output.append(HelpFormatter.formatTable(new String[][]{
                                new String[]{"Directives"},
                            }));
                    final List<List<String>> directiveTable = new ArrayList<List<String>>();
                    final List<String> firstRow = new ArrayList<String>();
                    firstRow.add("Key");
                    firstRow.add(" ");
                    firstRow.add("Default value");
                    firstRow.add(" ");
                    firstRow.add("Description");
                    directiveTable.add(firstRow);
                    Map<String, InstrumenterDirective> supportedDirectives = thisDescriptor.getRegisteredDirectives();
                    for (Entry<String, InstrumenterDirective> thisDirectiveEntry : supportedDirectives.entrySet()) {
                        final List<String> row = new ArrayList<String>();
                        InstrumenterDirective thisDirective = thisDirectiveEntry.getValue();

                        row.add(thisDirective.getKey());
                        row.add(" ");
                    
                        Object defaultValue = thisDirective.getDefaultValue(); 
                        if (defaultValue == null || thisDirective.getKey().equals("UUID")) { // TODO: UUID contains a random value as "default" value (and it is too long an messes up our table)
                            row.add("null");
                        } else {
                            row.add(defaultValue.toString() /* + " (" + defaultValue.getClass().getSimpleName() + ")" */); // TODO: Find a nice way to format the type information :-)
                        }
                        directiveTable.add(row);

                        row.add(" ");

                        row.add(thisDirective.getDescription());
                    }
                    criteraTable.add(Collections.<String>emptyList());
                    output.append(HelpFormatter.formatTable(directiveTable));
                }
    
                if (iterator.hasNext()) {
                output.append(HelpFormatter.formatTable(new String[][]{  
                            new String[]{},
                            new String[]{},
                            new String[]{HelpFormatter.getDividerWithLength(80, '-')}, // TODO: formatTable should do this for us...
                            new String[]{},
                            new String[]{},
                        }));
                }
            }
    
            System.out.println(output.toString());
            return 0;
        } else { // descriptors.size == 0
            if (pHasLanguage) {
                System.err.println("No instrumenters where found with the name " +
                                   pLanguage + "!");
                return 1;
            } else {
                System.err.println("No instrumenters where found!");
                return 1;
            }
        }
    }
}
