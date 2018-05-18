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

import java.util.*;

import org.codecover.batch.exceptions.*;

/**
 * A class for parsing options.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: OptionParser.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class OptionParser {
    private static void addValue(Map<Option, List<String>> values,
            Option option, String value) throws ParseException {
        List<String> list = values.get(option);
        if (list == null) {
            list = new ArrayList<String>();
            values.put(option, list);
        } else {
            if (!option.getCanAppearMultipleTimes()) {
                throw new OptionMultipleTimesException(option);
            }
        }
        list.add(value);
    }

    /**
     * Parse the given {@link List} of arguments with the given
     * {@link OptionSet}
     * 
     * @param options
     *                the given {@link OptionSet}
     * @param args
     *                the given {@link List} of arguments
     * @return the created {@link CommandLine}
     * @throws ParseException
     */
    public static CommandLine parse(OptionSet options, List<String> args)
            throws ParseException {
        final int length = args.size();
        int pos = 0;
        final List<String> nonOptionArgs = new ArrayList<String>();
        final Map<Option, List<String>> values = new HashMap<Option, List<String>>();

        while (pos < length) {
            final String str = args.get(pos);
            pos++;
            if (str.equals("--")) {
                while (pos < length) {
                    final String str2 = args.get(pos);
                    pos++;
                    if (str2 == null) {
                        throw new NullPointerException();
                    }
                    nonOptionArgs.add(str2);
                }
            } else if (str.startsWith("--")) {
                final String optionNameAndValue = str.substring(2);
                final int equalsPosition = optionNameAndValue.indexOf('=');
                final String optionName;
                if (equalsPosition == -1) {
                    optionName = optionNameAndValue;
                } else {
                    optionName = optionNameAndValue
                            .substring(0, equalsPosition);
                }
                final Option option = options.getByLongOption(optionName);
                if (option == null) {
                    throw new UnknownLongOptionException(optionName);
                }
                String argument = null;
                if (option.getHasArgument()) {
                    if (equalsPosition == -1) {
                        if (pos >= length) {
                            throw new MissingArgumentException(option, true);
                        }
                        argument = args.get(pos);
                        if (argument == null) {
                            throw new NullPointerException();
                        }
                        pos++;
                    } else {
                        argument = optionNameAndValue
                                .substring(equalsPosition + 1);
                    }
                } else {
                    if (equalsPosition != -1) {
                        throw new ErroneousArgumentException(option);
                    }
                }
                addValue(values, option, argument);
            } else if (str.startsWith("-") && str.length() > 1) {
                final int optionsLength = str.length();
                int optionsPos = 1;
                while (optionsPos < optionsLength) {
                    final char optionName = str.charAt(optionsPos);
                    optionsPos++;
                    final Option option = options.getByShortOption(optionName);
                    if (option == null) {
                        throw new UnknownShortOptionException(optionName);
                    }
                    String argument = null;
                    if (option.getHasArgument()) {
                        if (optionsPos < optionsLength) { // the argument is
                            // given directly
                            // ("svn info -r4")
                            argument = str.substring(optionsPos);
                            optionsPos = optionsLength;
                        } else { // the argument is given extra ("svn info -r
                            // 4")
                            if (pos >= length) {
                                throw new MissingArgumentException(option,
                                        false);
                            }
                            argument = args.get(pos);
                            if (argument == null) {
                                throw new NullPointerException();
                            }
                            pos++;
                        }
                    }
                    addValue(values, option, argument);
                }
            } else {
                nonOptionArgs.add(str);
            }
        }

        final Set<Option> missingOptions = new HashSet<Option>(options
                .getRequired());
        missingOptions.removeAll(values.keySet());
        if (!missingOptions.isEmpty()) {
            throw new MissingOptionException(missingOptions);
        }

        return new CommandLine(options, nonOptionArgs, values);
    }

    /**
     * Parses the given list of arguments and prints any errors, that occurred.
     * 
     * @param options
     *                the {@link OptionSet} to use in parsing
     * @param args
     *                the arguments to parse
     * @return the result of the parse operation, or <code>null</code>, if
     *         any error occurred.
     */
    public static CommandLine parseAndPrintError(OptionSet options,
            List<String> args) {
        try {
            return OptionParser.parse(options, args);
        } catch (UnknownOptionException e) {
            System.err.println("Unknown option: " + e.getOptionName());
            return null;
        } catch (MissingArgumentException e) {
            System.err.println("The option "
                    + e.getOption().getName(e.getIsLongOption())
                    + " needs an argument.");
            return null;
        } catch (ErroneousArgumentException e) {
            System.err.println("The option " + e.getOption().getName()
                    + " needs no argument.");
            return null;
        } catch (MissingOptionException e) {
            System.err.println(" the following "
                    + "options are required but not given: ");
            System.err.print(HelpFormatter.formatOptions(e.getOptions()));
            return null;
        } catch (OptionMultipleTimesException e) {
            System.err.println("The option " + e.getOption().getName()
                    + " is given multiple times.");
            return null;
        } catch (ParseException e) {
            // We should have caught all ParseExceptions.
            throw new RuntimeException("Should not get here.", e);
        }
    }
}
