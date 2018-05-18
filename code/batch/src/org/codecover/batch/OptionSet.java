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

import org.codecover.model.utils.CollectionUtil;

/**
 * Sets of required and optional {@link Option}s.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: OptionSet.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public final class OptionSet {
    private final Set<Option> required;

    private final Set<Option> optional;

    private final Set<Option> allOptions;

    private final Map<Character, Option> byShortName;

    private final Map<String, Option> byLongName;

    /**
     * An empty {@link OptionSet}.
     */
    public static final OptionSet EMPTY = new OptionSet(Collections
            .<Option> emptySet(), Collections.<Option> emptySet());

    private static void addOptions(Set<Option> options,
            Map<Character, Option> byShortName,
            final Map<String, Option> byLongName, Set<Option> myAllOptions) {
        for (Option option : options) {
            myAllOptions.add(option);
            char shortOption = option.getShortOption();
            if (shortOption != '\0') {
                if (byShortName.containsKey(shortOption)) {
                    throw new IllegalArgumentException("Short option '"
                            + shortOption + "' is used multiple times.");
                }
                byShortName.put(shortOption, option);
            }
            String longOption = option.getLongOption();
            if (longOption != null) {
                if (byLongName.containsKey(longOption)) {
                    // FIXME is something supposed to be done here?
                }
                byLongName.put(longOption, option);
            }
        }

    }

    /**
     * Constructor
     * 
     * @param required
     *                the options, that are required for use
     * @param optional
     *                the options, that are optional for use
     */
    public OptionSet(Set<Option> required, Set<Option> optional) {
        this.required = CollectionUtil.copy(required);
        this.optional = CollectionUtil.copy(optional);

        final Set<Option> myAllOptions = new HashSet<Option>();

        this.byShortName = new TreeMap<Character, Option>();
        this.byLongName = new TreeMap<String, Option>();

        addOptions(this.required, this.byShortName, this.byLongName,
                myAllOptions);
        addOptions(this.optional, this.byShortName, this.byLongName,
                myAllOptions);
        this.allOptions = Collections.unmodifiableSet(myAllOptions);
    }

    /**
     * Contructor
     * 
     * @param required
     *                the options, that are required for use
     * @param optional
     *                the options, that are optional for use
     */
    public OptionSet(Option[] required, Option[] optional) {
        this(new HashSet<Option>(Arrays.asList(required)), new HashSet<Option>(
                Arrays.asList(optional)));
    }

    /**
     * Gets the {@link Set} of required {@link Option}s
     * 
     * @return the set of required {@link Option}s
     */
    public Set<Option> getRequired() {
        return this.required;
    }

    /**
     * Gets the {@link Set} of optional {@link Option}s
     * 
     * @return the set of optional {@link Option}s
     */
    public Set<Option> getOptional() {
        return this.optional;
    }

    /**
     * Gets the {@link Set} of {@link Option}s
     * 
     * @return the set of {@link Option}s
     */
    public Set<Option> getAllOptions() {
        return this.allOptions;
    }

    /**
     * Gets an {@link Option} by its long name
     * 
     * @param longOption
     *                the long name
     * @return the {@link Option} with the given long name
     */
    public Option getByLongOption(String longOption) {
        return this.byLongName.get(longOption);
    }

    /**
     * Gets an {@link Option} by its short name
     * 
     * @param shortOption
     *                the short name
     * @return the {@link Option} with the given short name
     */
    public Option getByShortOption(char shortOption) {
        return this.byShortName.get(shortOption);
    }

    private static void mergeOption(Set<Option> options,
            Map<String, Option> longOptions,
            Map<Character, Option> shortOptions, Option optionToMerge) {
        final String longOption = optionToMerge.getLongOption();
        if (longOption != null) {
            final Option existingOption = longOptions.get(longOption);
            if (existingOption == null) {
                // No such option exists, create one
                final Option newOption = new Option('\0', longOption, null,
                        optionToMerge.getHasArgument(), null, true, Collections
                                .<Option> emptySet(), optionToMerge
                                .getHasArgument() ? new ArgumentType.Opaque()
                                : null);
                options.add(newOption);
                longOptions.put(longOption, newOption);
            } else {
                // Check whether existing option is ok
                if (existingOption.getHasArgument() != optionToMerge
                        .getHasArgument()) {
                    throw new IllegalArgumentException("The long option --"
                            + longOption
                            + " exists both with and without argument");
                }
            }
        }

        final char shortOption = optionToMerge.getShortOption();
        if (shortOption != '\0') {
            final Option existingOption = shortOptions.get(shortOption);
            if (existingOption == null) {
                // No such option exists, create one
                final Option newOption = new Option(shortOption, null, null,
                        optionToMerge.getHasArgument(), null, true, Collections
                                .<Option> emptySet(), optionToMerge
                                .getHasArgument() ? new ArgumentType.Opaque()
                                : null);
                options.add(newOption);
                shortOptions.put(shortOption, newOption);
            } else {
                // Check whether existing option is ok
                if (existingOption.getHasArgument() != optionToMerge
                        .getHasArgument()) {
                    throw new IllegalArgumentException("The short option -"
                            + shortOption
                            + " exists both with and without argument");
                }
            }
        }
    }

    /**
     * Create an {@code OptionSet} which contains every short or long option
     * which is on one of the original {@code OptionSet}s.
     * 
     * The hasArgument value for the option will be the same. No option will be
     * required. All options will be able to appear multiple times.
     * 
     * @param optionSets
     *                the {@link OptionSet}s to merge.
     * @return the merged {@link OptionSet}
     */
    public static OptionSet mergeAndReplace(Collection<OptionSet> optionSets) {
        final Set<Option> options = new HashSet<Option>();
        final Map<String, Option> longOptions = new HashMap<String, Option>();
        final Map<Character, Option> shortOptions = new HashMap<Character, Option>();

        for (OptionSet optionSet : optionSets) {
            for (Option option : optionSet.getRequired()) {
                mergeOption(options, longOptions, shortOptions, option);
            }
            for (Option option : optionSet.getOptional()) {
                mergeOption(options, longOptions, shortOptions, option);
            }
        }

        return new OptionSet(Collections.<Option> emptySet(), CollectionUtil
                .copy(options));
    }
}
