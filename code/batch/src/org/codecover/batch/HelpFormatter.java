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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Set;

/**
 * @author Steffen Kieß
 * @version 1.0 ($Id: HelpFormatter.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class HelpFormatter {
    /**
     * Wrap lines, replace \n by newline
     * 
     * @param string
     *            the {@link String} to wrap
     * @param spacesBefore
     *            the amount of whitespaces before the string
     * @return the wrapped {@link String}
     */
    public static String wrapLines(String string, int spacesBefore) {
        final int screenLength = 80; // TODO: don't hardcode this, try stuff like $COLUMNS

        final int length = screenLength - spacesBefore;

        if (string == null) {
            throw new NullPointerException("str == null");
        }

        if (length < 20) {
            // We have less than 20 chars left. give up.
            return string;
        }

        final String[] lines = string.split("\n", -1);

        final StringBuilder sb = new StringBuilder();

        for (int i = 0; i < lines.length; i++) {
            final String line = lines[i];

            int linePos = 0;

            while (linePos < line.length()) {
                if (length >= line.length() - linePos) {
                    // string will fit
                    sb.append(line.substring(linePos));
                    linePos = line.length();
                } else {
                    int whiteSpacePos = linePos + length;
                    while (whiteSpacePos >= linePos
                            && !Character.isWhitespace(line.charAt(whiteSpacePos))) {
                        whiteSpacePos--;
                    }
                    if (whiteSpacePos >= linePos) {
                        final int printedChars = whiteSpacePos - linePos + 1;
                        while (whiteSpacePos > linePos
                                && Character.isWhitespace(line.charAt(whiteSpacePos - 1))) {
                            whiteSpacePos--;
                        }
                        sb.append(line.substring(linePos, whiteSpacePos));
                        sb.append(System.getProperty("line.separator"));
                        for (int j = 0; j < spacesBefore; j++) {
                            sb.append(" ");
                        }
                        linePos += printedChars;
                    } else {
                        // no white space found, give up
                        sb.append(line.substring(linePos));
                    }
                }
            }

            if (i != lines.length - 1) {
                sb.append(System.getProperty("line.separator"));
                for (int j = 0; j < spacesBefore; j++) {
                    sb.append(" ");
                }
            }
        }

        return sb.toString();
    }

    /**
     * Formats the given array of arrays of {@link String}s
     * 
     * @param table
     *            the given array of arrays of {@link String}s
     * @return the formated String
     */
    public static String formatTable(String[][] table) {
        if (table == null) {
            throw new NullPointerException("table == null");
        }

        final List<List<String>> list = new ArrayList<List<String>>();
        
        for (final String[] a : table) {
            final List<String> list2 = new ArrayList<String>();
            for (final String s : a) {
                list2.add(s);
            }
            list.add(list2);
        }
        
        return formatTable(list);
    }
    
    //TODO: when a non-last column contains \n we should make a newline there,
    //      too :-$ (this is not very easy...)
    /**
     * Formats the given {@link List} of {@link List} of {@link String}s
     * 
     * @param table
     *            the given {@link List} of {@link List} of {@link String}s
     * @return the formated String
     */
    public static String formatTable(List<List<String>> table) {
        if (table == null) {
            throw new NullPointerException("table == null");
        }

        final StringBuilder sb = new StringBuilder();

        int columns = 0;
        final List<Integer> columnSizes = new ArrayList<Integer>();

        for (List<String> row : table) {
            if (row == null) {
                throw new NullPointerException("row == null");
            }
            final int rowLength = row.size();
            if (rowLength > columns) {
                columns = rowLength;
            }
            int i = 0;
            for (String cell : row) {
                if (cell == null) {
                    throw new NullPointerException("cell == null");
                }
                int columnSize = 0;
                if (columnSizes.size() > i) {
                    columnSize = columnSizes.get(i);
                } else {
                    columnSizes.add(0);
                }
                if (cell.length() > columnSize) {
                    columnSize = cell.length();
                }
                columnSizes.set(i, columnSize);
                i++;
            }
        }

        for (List<String> row : table) {
            int pos = 0;
            int i = 0;
            for (String cell : row) {
                if (i == columns - 1) {
                    sb.append(wrapLines(cell, pos));
                } else {
                    sb.append(cell);
                    final int columnSize = columnSizes.get(i);
                    for (int j = cell.length(); j < columnSize; j++) {
                        sb.append(" ");
                    }
                    pos += columnSize;
                }
                i++;
            }
            sb.append(System.getProperty("line.separator"));
        }

        return sb.toString();
    }

    /**
     * Formats the given {@link Option}
     * 
     * @param option
     *            the given {@link Option}
     * @return the list of formatted Strings
     */
    public static List<String> formatOption(Option option) {
        final List<String> line = new ArrayList<String>();

        line.add(" ");

        char shortOption = option.getShortOption();
        String longOption = option.getLongOption();

        if (shortOption == '\0') {
            line.add("");
        } else {
            String comma = "";
            if (longOption != null) {
                comma = ",";
            }
            line.add("-" + shortOption + comma + " ");
        }

        if (longOption == null) {
            line.add("");
        } else {
            line.add("--" + longOption);
        }

        line.add("  ");

        line.add(option.getDescription());

        return line;
    }

    /**
     * Formats the given {@link Set} of {@link Option}s
     * 
     * @param options
     *            the given {@link Set} of {@link Option}s
     * @return the formatted {@link String}
     */
    public static String formatOptions(Set<Option> options) {
        final List<List<String>> optionLines = new ArrayList<List<String>>();

        final List<Option> optionList = new ArrayList<Option>(options);
        Collections.sort(optionList, new Comparator<Option>() {
            public int compare(Option o1, Option o2) {
                String name1 = o1.getLongOption();
                String name2 = o2.getLongOption();

                if (name1 == null) {
                    name1 = "";
                }

                if (name2 == null) {
                    name2 = "";
                }

                int result = name1.compareTo(name2);

                if (result == 0) {
                    result = new Character(o1.getShortOption()).compareTo(o2.getShortOption());
                }

                return result;
            }
        });
        for (Option option : optionList) {
            optionLines.add(formatOption(option));
        }

        return formatTable(optionLines);
    }

    /**
     * Formats the given {@link Command}
     * 
     * @param command
     *            the given {@link Command}
     * @return the formatted {@link String}
     */
    public static String formatCommand(Command command) {
        final StringBuilder sb = new StringBuilder();

        sb.append("usage: codecover ");
        sb.append(command.getName());
        if (command.getOptions().getRequired().size() > 0) {
            sb.append(" <options>");
        } else {
            if (command.getOptions().getOptional().size() > 0) {
                sb.append(" [options]");
            }
        }
        if (command.getMaximumArguments() != 0) {
            if (command.getMinimumArguments() > 0) {
                sb.append(" <arguments>");
            } else {
                sb.append(" [arguments]");
            }
        }
        sb.append(System.getProperty("line.separator"));

        sb.append(wrapLines(command.getDetailedDescription(), 0));
        sb.append(System.getProperty("line.separator"));

        if (!command.getOptions().getRequired().isEmpty()) {
            sb.append(System.getProperty("line.separator"));
            sb.append("REQUIRED options:");
            sb.append(System.getProperty("line.separator"));
            sb.append(formatOptions(command.getOptions().getRequired()));
        }

        if (!command.getOptions().getOptional().isEmpty()) {
            sb.append(System.getProperty("line.separator"));
            sb.append("OPTIONAL options:");
            sb.append(System.getProperty("line.separator"));
            sb.append(formatOptions(command.getOptions().getOptional()));
        }

        return sb.toString();
    }

    /**
     * Formats a {@link String} to hold the information about the usage of
     * codecover
     * 
     * @param commands
     *            the {@link CommandCollection} containing the {@link Command}s
     * @param optionsValidWithoutCommand
     *            the set of {@link Options}, that can be used without a
     *            {@link Command}
     * @return the formatted {@link String}
     */
    public static String formatUsage(CommandCollection commands,
            Set<Option> optionsValidWithoutCommand) {
        final StringBuilder sb = new StringBuilder();

        sb.append("usage: codecover <subcommand> [options] [args]");
        sb.append(System.getProperty("line.separator"));

        final List<List<String>> commandLines = new ArrayList<List<String>>();
        for (Command command : commands.getSortedCommands()) {
            if (command.isHidden()) {
                continue;
            }

            final List<String> line = new ArrayList<String>();
            commandLines.add(line);

            line.add(" ");

            String shortName = command.getShortName();
            if (shortName == null) {
                line.add("");
            } else {
                line.add(shortName + ", ");
            }

            line.add(command.getName());

            line.add("  ");

            line.add(command.getDescription());
        }
        sb.append(formatTable(commandLines));

        sb.append(System.getProperty("line.separator"));
        sb.append(System.getProperty("line.separator"));

        sb.append("Or one of the following options without a command:");
        sb.append(System.getProperty("line.separator"));

        sb.append(System.getProperty("line.separator"));

        sb.append("usage: codecover <option>");
        sb.append(System.getProperty("line.separator"));

        sb.append(formatOptions(optionsValidWithoutCommand));

        sb.append(System.getProperty("line.separator"));

        return sb.toString();
    }

    /**
     * Creates a String of the following kind
     * <p>
     * <code>ccccccc</code>
     * <p>
     * with the length of the given number
     * 
     * @param length
     *            the number of dashes
     * @param c
     *            the character to be used in the divider
     * @return the demanded amount of dashes in one string.
     */
    public static String getDividerWithLength(int length, char c) {
        final StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) {
            sb.append(c);
        }
        return sb.toString();
    }
}
