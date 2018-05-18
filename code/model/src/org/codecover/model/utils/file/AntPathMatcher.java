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

/*
 * Copyright 2002-2007 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.codecover.model.utils.file;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.regex.Pattern;

/**
 * PathMatcher implementation for Ant-style path patterns.
 * Examples are provided below.
 *
 * <p>Part of this mapping code has been kindly borrowed from
 * <a href="http://ant.apache.org">Apache Ant</a>.
 *
 * <p>The mapping matches URLs using the following rules:<br>
 * <ul>
 * <li>? matches one character</li>
 * <li>* matches zero or more characters</li>
 * <li>** matches zero or more 'directories' in a path</li>
 * </ul>
 *
 * <p>Some examples:<br>
 * <ul>
 * <li><code>com/t?st.jsp</code> - matches <code>com/test.jsp</code> but also
 * <code>com/tast.jsp</code> or <code>com/txst.jsp</code></li>
 * <li><code>com/*.jsp</code> - matches all <code>.jsp</code> files in the
 * <code>com</code> directory</li>
 * <li><code>com/&#42;&#42;/test.jsp</code> - matches all <code>test.jsp</code>
 * files underneath the <code>com</code> path</li>
 * <li><code>org/springframework/&#42;&#42;/*.jsp</code> - matches all <code>.jsp</code>
 * files underneath the <code>org/springframework</code> path</li>
 * <li><code>org/&#42;&#42;/servlet/bla.jsp</code> - matches
 * <code>org/springframework/servlet/bla.jsp</code> but also
 * <code>org/springframework/testing/servlet/bla.jsp</code> and
 * <code>org/servlet/bla.jsp</code></li>
 * </ul>
 *
 * @author Alef Arendsen
 * @author Juergen Hoeller
 * @author Rob Harrop
 * @author Christoph Müller<br>
 *   Thankfully copied at 27.06.2007 from the 
 *   <a href="http://springframework.cvs.sourceforge.net/springframework/spring/src/org/springframework/util/">springframework</a>:
 *   <ul>
 *   <li>{@link AntPathMatcher} from <code>org.springframework.util.AntPathMatcher rev. 1.15</code></li>
 *   <li>{@link #tokenizeToStringArray(String, String, boolean)} from <code>org.springframework.util.StringUtils rev. 1.6.0</code> StringUtils.java)</li>
 *   </ul>
 *
 *   Thankfully copied at 28.06.2007 from apache ant v1.7.0:
 *   <ul>
 *   <li>{@link #normalizePattern(String)} from <code>org.apache.tools.ant.DirectoryScanner</code></li>
 *   </ul>
 * @version 1.0 ($Id: AntPathMatcher.java 1 2007-12-12 17:37:26Z t-scheller $)
 */
public class AntPathMatcher {

    /**
     * Instead of using {@link File#separatorChar}, we use a hard binding to
     * <code>/</code>.
     */
    public static final char SEPARATOR_CHAR = '/';
    
    /**
     * Instead of using {@link File#separatorChar}, we use a hard binding to
     * <code>/</code>.
     * 
     * @see #SEPARATOR_CHAR
     */
    public static final String SEPARATOR_STRING = Character.toString(SEPARATOR_CHAR);
  
    /** all <code>**&#47;**&#47;**</code> can be replaced by <code>**</code> */
    public static final Pattern REDUCING_PATTERN = Pattern.compile("\\*\\*(/\\*\\*)+");  

    /**
     * This is a cache for {@link #tokenizeToStringArray(String, String, boolean)},
     * where a cache is used just for patterns.
     */
    private Map<String, String[]> tokenizedCache = new HashMap<String, String[]>();

    /**
     * Does the given <code>path</code> represent a pattern that can be matched
     * by an implementation of this interface?
     * <p>If the return value is <code>false</code>, then the {@link #match}
     * method does not have to be used because direct equality comparisons
     * on the static path Strings will lead to the same result.
     * @param path the path String to check
     * @return <code>true</code> if the given <code>path</code> represents a pattern
     */
    public static boolean isPattern(String path) {
        return (path.indexOf('*') != -1 || path.indexOf('?') != -1);
    }

    /**
     * All '\' characters are replaced by <code>/</code>, so the separator used
     * needs not match any fixed character.
     *
     * <p> When a pattern ends with a '/' or '\', "**" is appended.
     * 
     * @param pattern
     *          The pattern to normalize. 
     * 
     * @return The normalized String. 
     *
     * @since Ant 1.6.3
     */
    public String normalizePattern(String pattern) {
        String newPattern = pattern;
        // we normalize the pattern by using only / on Windows and Linux
        newPattern = newPattern.replace('\\', SEPARATOR_CHAR);
        // we replace **/**/** by **, because it has the same semantic
        newPattern = REDUCING_PATTERN.matcher(newPattern).replaceAll("**");
        // if a path ends with / wee add **, because we want to have all files
        // in the folder too
        if (newPattern.length() > 0 && newPattern.charAt(newPattern.length() - 1) == SEPARATOR_CHAR) {
            newPattern += "**";
        }
        return newPattern;
    }

    /**
     * All '\' characters are replaced by <code>/</code>, so the separator used
     * needs not match any fixed character.
     * 
     * @param path
     *          The path to normalize. 
     * 
     * @return The normalized String. 
     */
    public String normalizePath(String path) {
      if (path.indexOf('\\') == -1) {
        return path;
      }
      return path.replace('\\', SEPARATOR_CHAR);
    }

    /**
     * Match the given <code>path</code> against the given <code>pattern</code>,
     * according to this PathMatcher's matching strategy.
     * @param pattern the pattern to match against
     * @param path the path String to test
     * @return <code>true</code> if the supplied <code>path</code> matched,
     * <code>false</code> if it didn't
     */
    public boolean match(String pattern, String path) {
        return doMatch(pattern, path, true);
    }

    /**
     * Match the given <code>path</code> against the corresponding part of the given
     * <code>pattern</code>, according to this PathMatcher's matching strategy.
     * <p>Determines whether the pattern at least matches as far as the given base
     * path goes, assuming that a full path may then match as well.
     * @param pattern the pattern to match against
     * @param path the path String to test
     * @return <code>true</code> if the supplied <code>path</code> matched,
     * <code>false</code> if it didn't
     */
    public boolean matchStart(String pattern, String path) {
        return doMatch(pattern, path, false);
    }

    /**
     * Actually match the given <code>path</code> against the given <code>pattern</code>.<br>
     * <br>
     * A cache is used for the tokenized patterns.
     * 
     * @param pattern the pattern to match against
     * @param path the path String to test
     * @param fullMatch whether a full pattern match is required
     * (else a pattern match as far as the given base path goes is sufficient)
     * @return <code>true</code> if the supplied <code>path</code> matched,
     * <code>false</code> if it didn't
     */
    public boolean doMatch(String pattern, String path, boolean fullMatch) {
        // if it is a pattern without wildcards and we need a full match, we can
        // use string.equals instead
        if (fullMatch && !isPattern(pattern)) {
            return pattern.equals(path);
        }

        if (path.startsWith(SEPARATOR_STRING) != pattern.startsWith(SEPARATOR_STRING)) {
            return false;
        }

        String[] pattDirs = tokenizeToStringArray(pattern, SEPARATOR_STRING, true);
        String[] pathDirs = tokenizeToStringArray(path, SEPARATOR_STRING, false);

        int pattIdxStart = 0;
        int pattIdxEnd = pattDirs.length - 1;
        int pathIdxStart = 0;
        int pathIdxEnd = pathDirs.length - 1;

        // Match all elements up to the first **
        while (pattIdxStart <= pattIdxEnd && pathIdxStart <= pathIdxEnd) {
            String pattDir = pattDirs[pattIdxStart];
            if ("**".equals(pattDir)) {
                break;
            }
            if (!matchStrings(pattDir, pathDirs[pathIdxStart])) {
                return false;
            }
            pattIdxStart++;
            pathIdxStart++;
        }

        if (pathIdxStart > pathIdxEnd) {
            // Path is exhausted, only match if rest of pattern is * or **'s
            if (pattIdxStart > pattIdxEnd) {
                return (pattern.endsWith(SEPARATOR_STRING) ?
                        path.endsWith(SEPARATOR_STRING) : !path.endsWith(SEPARATOR_STRING));
            }
            if (!fullMatch) {
                return true;
            }
            if (pattIdxStart == pattIdxEnd && pattDirs[pattIdxStart].equals("*") &&
                    path.endsWith(SEPARATOR_STRING)) {
                return true;
            }
            for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
                if (!pattDirs[i].equals("**")) {
                    return false;
                }
            }
            return true;
        }
        else if (pattIdxStart > pattIdxEnd) {
            // String not exhausted, but pattern is. Failure.
            return false;
        }
        else if (!fullMatch && "**".equals(pattDirs[pattIdxStart])) {
            // Path start definitely matches due to "**" part in pattern.
            return true;
        }

        // up to last '**'
        while (pattIdxStart <= pattIdxEnd && pathIdxStart <= pathIdxEnd) {
            String patDir = pattDirs[pattIdxEnd];
            if (patDir.equals("**")) {
                break;
            }
            if (!matchStrings(patDir, pathDirs[pathIdxEnd])) {
                return false;
            }
            pattIdxEnd--;
            pathIdxEnd--;
        }
        if (pathIdxStart > pathIdxEnd) {
            // String is exhausted
            for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
                if (!pattDirs[i].equals("**")) {
                    return false;
                }
            }
            return true;
        }

        while (pattIdxStart != pattIdxEnd && pathIdxStart <= pathIdxEnd) {
            int patIdxTmp = -1;
            for (int i = pattIdxStart + 1; i <= pattIdxEnd; i++) {
                if (pattDirs[i].equals("**")) {
                    patIdxTmp = i;
                    break;
                }
            }
            if (patIdxTmp == pattIdxStart + 1) {
                // '**/**' situation, so skip one
                pattIdxStart++;
                continue;
            }
            // Find the pattern between padIdxStart & padIdxTmp in str between
            // strIdxStart & strIdxEnd
            int patLength = (patIdxTmp - pattIdxStart - 1);
            int strLength = (pathIdxEnd - pathIdxStart + 1);
            int foundIdx = -1;

            strLoop:
                for (int i = 0; i <= strLength - patLength; i++) {
                    for (int j = 0; j < patLength; j++) {
                        String subPat = pattDirs[pattIdxStart + j + 1];
                        String subStr = pathDirs[pathIdxStart + i + j];
                        if (!matchStrings(subPat, subStr)) {
                            continue strLoop;
                        }
                    }
                    foundIdx = pathIdxStart + i;
                    break;
                }

            if (foundIdx == -1) {
                return false;
            }

            pattIdxStart = patIdxTmp;
            pathIdxStart = foundIdx + patLength;
        }

        for (int i = pattIdxStart; i <= pattIdxEnd; i++) {
            if (!pattDirs[i].equals("**")) {
                return false;
            }
        }

        return true;
    }

    /**
     * Tests whether or not a string matches against a pattern.
     * The pattern may contain two special characters:<br>
     * '*' means zero or more characters<br>
     * '?' means one and only one character
     * @param pattern pattern to match against.
     * Must not be <code>null</code>.
     * @param str string which must be matched against the pattern.
     * Must not be <code>null</code>.
     * @return <code>true</code> if the string matches against the
     * pattern, or <code>false</code> otherwise.
     */
    boolean matchStrings(String pattern, String str) {
        char[] patArr = pattern.toCharArray();
        char[] strArr = str.toCharArray();
        int patIdxStart = 0;
        int patIdxEnd = patArr.length - 1;
        int strIdxStart = 0;
        int strIdxEnd = strArr.length - 1;
        char ch;

        boolean containsStar = false;
        for (int i = 0; i < patArr.length; i++) {
            if (patArr[i] == '*') {
                containsStar = true;
                break;
            }
        }

        if (!containsStar) {
            // No '*'s, so we make a shortcut
            if (patIdxEnd != strIdxEnd) {
                return false; // Pattern and string do not have the same size
            }
            for (int i = 0; i <= patIdxEnd; i++) {
                ch = patArr[i];
                if (ch != '?') {
                    if (ch != strArr[i]) {
                        return false;// Character mismatch
                    }
                }
            }
            return true; // String matches against pattern
        }


        if (patIdxEnd == 0) {
            return true; // Pattern contains only '*', which matches anything
        }

        // Process characters before first star
        while ((ch = patArr[patIdxStart]) != '*' && strIdxStart <= strIdxEnd) {
            if (ch != '?') {
                if (ch != strArr[strIdxStart]) {
                    return false;// Character mismatch
                }
            }
            patIdxStart++;
            strIdxStart++;
        }
        if (strIdxStart > strIdxEnd) {
            // All characters in the string are used. Check if only '*'s are
            // left in the pattern. If so, we succeeded. Otherwise failure.
            for (int i = patIdxStart; i <= patIdxEnd; i++) {
                if (patArr[i] != '*') {
                    return false;
                }
            }
            return true;
        }

        // Process characters after last star
        while ((ch = patArr[patIdxEnd]) != '*' && strIdxStart <= strIdxEnd) {
            if (ch != '?') {
                if (ch != strArr[strIdxEnd]) {
                    return false;// Character mismatch
                }
            }
            patIdxEnd--;
            strIdxEnd--;
        }
        if (strIdxStart > strIdxEnd) {
            // All characters in the string are used. Check if only '*'s are
            // left in the pattern. If so, we succeeded. Otherwise failure.
            for (int i = patIdxStart; i <= patIdxEnd; i++) {
                if (patArr[i] != '*') {
                    return false;
                }
            }
            return true;
        }

        // process pattern between stars. padIdxStart and patIdxEnd point
        // always to a '*'.
        while (patIdxStart != patIdxEnd && strIdxStart <= strIdxEnd) {
            int patIdxTmp = -1;
            for (int i = patIdxStart + 1; i <= patIdxEnd; i++) {
                if (patArr[i] == '*') {
                    patIdxTmp = i;
                    break;
                }
            }
            if (patIdxTmp == patIdxStart + 1) {
                // Two stars next to each other, skip the first one.
                patIdxStart++;
                continue;
            }
            // Find the pattern between padIdxStart & padIdxTmp in str between
            // strIdxStart & strIdxEnd
            int patLength = (patIdxTmp - patIdxStart - 1);
            int strLength = (strIdxEnd - strIdxStart + 1);
            int foundIdx = -1;
            strLoop:
                for (int i = 0; i <= strLength - patLength; i++) {
                    for (int j = 0; j < patLength; j++) {
                        ch = patArr[patIdxStart + j + 1];
                        if (ch != '?') {
                            if (ch != strArr[strIdxStart + i + j]) {
                                continue strLoop;
                            }
                        }
                    }

                    foundIdx = strIdxStart + i;
                    break;
                }

            if (foundIdx == -1) {
                return false;
            }

            patIdxStart = patIdxTmp;
            strIdxStart = foundIdx + patLength;
        }

        // All characters in the string are used. Check if only '*'s are left
        // in the pattern. If so, we succeeded. Otherwise failure.
        for (int i = patIdxStart; i <= patIdxEnd; i++) {
            if (patArr[i] != '*') {
                return false;
            }
        }

        return true;
    }

    /**
     * Tokenize the given String into a String array via a StringTokenizer.
     * Trims tokens and omits empty tokens.
     * <p>
     * The given delimiters string is supposed to consist of any number of
     * delimiter characters. Each of those characters can be used to separate
     * tokens. A delimiter is always a single character.
     * 
     * @param str
     *            the String to tokenize
     * @param delimiters
     *            the delimiter characters, assembled as String (each of those
     *            characters is individually considered as delimiter).
     * @param useCache use the cache for this string?
     * @return an array of the tokens
     * @see java.util.StringTokenizer
     * @see java.lang.String#trim()
     */
    String[] tokenizeToStringArray(String str, String delimiters,
            boolean useCache) {
        // do we have it in the cache?
        String[] ret = this.tokenizedCache.get(str);
        if (ret != null) {
            return ret;
        }

        StringTokenizer st = new StringTokenizer(str, delimiters);
        List<String> tokens = new ArrayList<String>(8);
        while (st.hasMoreTokens()) {
            String token = st.nextToken();
            token = token.trim();
            tokens.add(token);
        }

        ret = tokens.toArray(new String[tokens.size()]);
        if (useCache) {
            this.tokenizedCache.put(str, ret);
        }
        return ret;
    }

    /**
     * Cleares {@link #tokenizedCache}.
     */
    public void clearCache() {
        this.tokenizedCache.clear();
    }
}
