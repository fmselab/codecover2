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

import org.codecover.model.utils.*;

/**
 * The type of an argument (of a {@link Command} or a {@link Option}).
 * 
 * Right now this type is only a suggestion and is not checked at any time.
 * 
 * @author Steffen Kieß
 * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public abstract class ArgumentType {
    /**
     * An {@link ArgumentType} for a path.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class Path extends ArgumentType {
        /* empty class. */
    }

    /**
     * An {@link ArgumentType} for a directory.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class Directory extends Path {
        /* empty class. */
    }

    /**
     * An {@link ArgumentType} for a file.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class File extends Path {
        /* empty class. */
    }

    /**
     * An {@link ArgumentType} for a number.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class Number extends ArgumentType {
        /* empty class. */
    }

    /**
     * An {@link ArgumentType} for opaque.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class Opaque extends ArgumentType {
        /* empty class. */
    }

    /**
     * An {@link ArgumentType} for a suggestion list.
     * 
     * @author Steffen Kieß
     * @version 1.0 ($Id: ArgumentType.java 1 2007-12-12 17:37:26Z t-scheller $)
     */
    public static class SuggestionList extends ArgumentType {
        private final List<String> suggestions;

        /**
         * Constructor.
         * 
         * @param suggestions
         *                a list of suggestion strings.
         */
        public SuggestionList(List<String> suggestions) {
            this.suggestions = CollectionUtil.copy(suggestions);
        }

        /**
         * Constructor.
         * 
         * @param suggestions
         *                an array of suggestion strings.
         */
        public SuggestionList(String... suggestions) {
            final List<String> list = new ArrayList<String>();
            for (String s : suggestions) {
                list.add(s);
            }
            this.suggestions = Collections.unmodifiableList(list);
        }

        /**
         * Gets the suggestions.
         * 
         * @return the suggestions
         */
        public List<String> getSuggestions() {
            return this.suggestions;
        }
    }
}
